{-# LANGUAGE OverloadedStrings #-}

module Pllisp.Type where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- CORE

type Symbol = T.Text

data Typed a = Typed
  { ty  :: Type
  , val :: a
  } deriving (Eq, Show)

data Type
  = TyInt
  | TyFlt
  | TyStr
  | TyBool
  | TyUnit
  | TyRx
  | TyUSym
  | TySyntax
  | TyFun [Type] Type
  | TyCon Symbol [Type]  -- user-defined: TyCon "Maybe" [TyInt]
  | TyApp Type Type      -- HKT application: TyApp (TyVar 0) TyInt
  | TyVar Integer
  deriving (Eq, Show)

-- C types for FFI declarations. These represent the actual C ABI types
-- (which may differ from pllisp's native LLVM types).
data CType
  = CI8 | CI16 | CI32 | CI64
  | CU8 | CU16 | CU32 | CU64
  | CF32 | CF64
  | CPtr
  | CVoid
  | CArr !Int CType       -- fixed-size inline array: CArr count elemType
  | CStruct T.Text        -- reference to a named struct (for nested structs)
  deriving (Eq, Show)

-- The pllisp type that a CType maps to for the purposes of the type checker.
cTypeToPllisp :: CType -> Type
cTypeToPllisp CI8  = TyInt
cTypeToPllisp CI16 = TyInt
cTypeToPllisp CI32 = TyInt
cTypeToPllisp CI64 = TyInt
cTypeToPllisp CU8  = TyInt
cTypeToPllisp CU16 = TyInt
cTypeToPllisp CU32 = TyInt
cTypeToPllisp CU64 = TyInt
cTypeToPllisp CF32 = TyFlt
cTypeToPllisp CF64 = TyFlt
cTypeToPllisp CPtr = TyStr
cTypeToPllisp CVoid = TyUnit
cTypeToPllisp (CArr _ _) = TyStr  -- array fields accessed as pointers
cTypeToPllisp (CStruct name) = TyCon name []  -- nested struct type

-- Numeric tag for CType, used by the libffi bridge.
-- Must match the tag_to_ffi_type array in ffiBridgeC.
cTypeTag :: CType -> Int
cTypeTag CI8  = 0
cTypeTag CI16 = 1
cTypeTag CI32 = 2
cTypeTag CI64 = 3
cTypeTag CU8  = 4
cTypeTag CU16 = 5
cTypeTag CU32 = 6
cTypeTag CU64 = 7
cTypeTag CF32 = 8
cTypeTag CF64 = 9
cTypeTag CPtr = 10
cTypeTag CVoid = 11
cTypeTag (CArr _ _) = 10  -- treated as pointer
cTypeTag (CStruct _) = 10  -- treated as pointer

-- The C source for the libffi bridge runtime.
-- Compiled alongside the generated LLVM IR.
ffiBridgeC :: T.Text
ffiBridgeC = T.unlines
  [ "#include <ffi.h>"
  , "#include <stdint.h>"
  , "#include <stdlib.h>"
  , ""
  , "static ffi_type* tag_to_ffi_type(int tag) {"
  , "    static ffi_type* types[] = {"
  , "        &ffi_type_sint8,   /* 0  CI8  */"
  , "        &ffi_type_sint16,  /* 1  CI16 */"
  , "        &ffi_type_sint32,  /* 2  CI32 */"
  , "        &ffi_type_sint64,  /* 3  CI64 */"
  , "        &ffi_type_uint8,   /* 4  CU8  */"
  , "        &ffi_type_uint16,  /* 5  CU16 */"
  , "        &ffi_type_uint32,  /* 6  CU32 */"
  , "        &ffi_type_uint64,  /* 7  CU64 */"
  , "        &ffi_type_float,   /* 8  CF32 */"
  , "        &ffi_type_double,  /* 9  CF64 */"
  , "        &ffi_type_pointer, /* 10 CPtr */"
  , "        &ffi_type_void,    /* 11 CVoid */"
  , "    };"
  , "    return types[tag];"
  , "}"
  , ""
  , "void pll_ffi_call(void *fn, int nargs, int *arg_types,"
  , "                  int ret_type, void **arg_vals, void *ret_val) {"
  , "    ffi_cif cif;"
  , "    ffi_type *atypes[nargs > 0 ? nargs : 1];"
  , "    for (int i = 0; i < nargs; i++)"
  , "        atypes[i] = tag_to_ffi_type(arg_types[i]);"
  , "    ffi_type *rtype = tag_to_ffi_type(ret_type);"
  , "    ffi_prep_cif(&cif, FFI_DEFAULT_ABI, (unsigned)nargs,"
  , "                 rtype, nargs > 0 ? atypes : NULL);"
  , "    ffi_call(&cif, FFI_FN(fn), ret_val, nargs > 0 ? arg_vals : NULL);"
  , "}"
  , ""
  , "void pll_ffi_call_var(void *fn, int nfixed, int nargs, int *arg_types,"
  , "                      int ret_type, void **arg_vals, void *ret_val) {"
  , "    ffi_cif cif;"
  , "    ffi_type *atypes[nargs > 0 ? nargs : 1];"
  , "    for (int i = 0; i < nargs; i++)"
  , "        atypes[i] = tag_to_ffi_type(arg_types[i]);"
  , "    ffi_type *rtype = tag_to_ffi_type(ret_type);"
  , "    ffi_prep_cif_var(&cif, FFI_DEFAULT_ABI, (unsigned)nfixed, (unsigned)nargs,"
  , "                     rtype, nargs > 0 ? atypes : NULL);"
  , "    ffi_call(&cif, FFI_FN(fn), ret_val, nargs > 0 ? arg_vals : NULL);"
  , "}"
  , ""
  , "/* Callback support: wraps a pllisp closure into a C function pointer. */"
  , "/* The closure layout is: [fnptr, env0, env1, ...] */"
  , "/* The generated C callback calls fnptr(closure_ptr, c_arg0, c_arg1, ...) */"
  , ""
  , "typedef struct {"
  , "    void *closure;       /* pllisp closure pointer */"
  , "    int nargs;           /* number of C args */"
  , "    int *arg_types;      /* CType tags for C args */"
  , "    int ret_type;        /* CType tag for C return */"
  , "    ffi_cif inner_cif;   /* CIF for calling pllisp function */"
  , "} pll_cb_data;"
  , ""
  , "static void pll_cb_handler(ffi_cif *cif, void *ret, void **args, void *user_data) {"
  , "    pll_cb_data *data = (pll_cb_data *)user_data;"
  , "    void *closure = data->closure;"
  , "    int nargs = data->nargs;"
  , ""
  , "    /* Load fnptr from closure[0] */"
  , "    void *fnptr = *(void **)closure;"
  , ""
  , "    /* Build pllisp args: [closure_ptr, arg0_as_i64, ...] */"
  , "    int total_args = nargs + 1;"
  , "    void *pll_arg_ptrs[total_args];"
  , "    int64_t converted[nargs > 0 ? nargs : 1];"
  , "    pll_arg_ptrs[0] = &closure;"
  , "    for (int i = 0; i < nargs; i++) {"
  , "        switch (data->arg_types[i]) {"
  , "            case 0:  converted[i] = (int64_t)*(int8_t *)args[i]; break;"
  , "            case 1:  converted[i] = (int64_t)*(int16_t *)args[i]; break;"
  , "            case 2:  converted[i] = (int64_t)*(int32_t *)args[i]; break;"
  , "            case 3:  converted[i] = *(int64_t *)args[i]; break;"
  , "            case 4:  converted[i] = (int64_t)*(uint8_t *)args[i]; break;"
  , "            case 5:  converted[i] = (int64_t)*(uint16_t *)args[i]; break;"
  , "            case 6:  converted[i] = (int64_t)*(uint32_t *)args[i]; break;"
  , "            case 7:  converted[i] = (int64_t)*(uint64_t *)args[i]; break;"
  , "            case 10: converted[i] = (int64_t)(intptr_t)*(void **)args[i]; break;"
  , "            default: converted[i] = *(int64_t *)args[i]; break;"
  , "        }"
  , "        pll_arg_ptrs[i + 1] = &converted[i];"
  , "    }"
  , ""
  , "    /* Call pllisp function via libffi */"
  , "    int64_t result;"
  , "    ffi_call(&data->inner_cif, FFI_FN(fnptr), &result, pll_arg_ptrs);"
  , ""
  , "    /* Convert result back to C type */"
  , "    switch (data->ret_type) {"
  , "        case 0:  *(int8_t *)ret = (int8_t)result; break;"
  , "        case 1:  *(int16_t *)ret = (int16_t)result; break;"
  , "        case 2:  *(int32_t *)ret = (int32_t)result; break;"
  , "        case 3:  *(int64_t *)ret = result; break;"
  , "        case 4:  *(uint8_t *)ret = (uint8_t)result; break;"
  , "        case 5:  *(uint16_t *)ret = (uint16_t)result; break;"
  , "        case 6:  *(uint32_t *)ret = (uint32_t)result; break;"
  , "        case 7:  *(uint64_t *)ret = (uint64_t)result; break;"
  , "        case 10: *(void **)ret = (void *)(intptr_t)result; break;"
  , "        default: *(int64_t *)ret = result; break;"
  , "    }"
  , "}"
  , ""
  , "void *pll_make_callback(void *closure, int nargs, int *arg_types, int ret_type) {"
  , "    /* Build outer CIF (what C callers see) */"
  , "    ffi_type **outer_atypes = malloc(sizeof(ffi_type*) * (nargs > 0 ? nargs : 1));"
  , "    for (int i = 0; i < nargs; i++)"
  , "        outer_atypes[i] = tag_to_ffi_type(arg_types[i]);"
  , "    ffi_type *outer_rtype = tag_to_ffi_type(ret_type);"
  , ""
  , "    ffi_cif *outer_cif = malloc(sizeof(ffi_cif));"
  , "    ffi_prep_cif(outer_cif, FFI_DEFAULT_ABI, (unsigned)nargs,"
  , "                 outer_rtype, nargs > 0 ? outer_atypes : NULL);"
  , ""
  , "    /* Build inner CIF (pllisp calling convention: ptr + i64 args -> i64) */"
  , "    pll_cb_data *data = malloc(sizeof(pll_cb_data));"
  , "    data->closure = closure;"
  , "    data->nargs = nargs;"
  , "    data->arg_types = arg_types;"
  , "    data->ret_type = ret_type;"
  , ""
  , "    int inner_nargs = nargs + 1;"
  , "    ffi_type **inner_atypes = malloc(sizeof(ffi_type*) * inner_nargs);"
  , "    inner_atypes[0] = &ffi_type_pointer;"
  , "    for (int i = 0; i < nargs; i++)"
  , "        inner_atypes[i + 1] = &ffi_type_sint64;"
  , "    ffi_prep_cif(&data->inner_cif, FFI_DEFAULT_ABI, (unsigned)inner_nargs,"
  , "                 &ffi_type_sint64, inner_atypes);"
  , ""
  , "    /* Allocate libffi closure */"
  , "    void *code_ptr;"
  , "    ffi_closure *ffi_cl = ffi_closure_alloc(sizeof(ffi_closure), &code_ptr);"
  , "    ffi_prep_closure_loc(ffi_cl, outer_cif, pll_cb_handler, data, code_ptr);"
  , "    return code_ptr;"
  , "}"
  , ""
  , "/* Test helper: apply a C function pointer (int64_t -> int64_t) */"
  , "int64_t pll_test_apply_int(void *fn, int64_t x) {"
  , "    typedef int64_t (*fn_t)(int64_t);"
  , "    return ((fn_t)fn)(x);"
  , "}"
  , ""
  , "/* Test helpers for struct by value */"
  , "typedef struct { int32_t x; int32_t y; } pll_point_t;"
  , ""
  , "int64_t pll_point_sum(void *p) {"
  , "    pll_point_t *pt = (pll_point_t *)p;"
  , "    return (int64_t)(pt->x + pt->y);"
  , "}"
  , ""
  , "void pll_point_scale(void *p, int64_t factor) {"
  , "    pll_point_t *pt = (pll_point_t *)p;"
  , "    pt->x *= (int32_t)factor;"
  , "    pt->y *= (int32_t)factor;"
  , "}"
  ]

-- The LLVM IR type string for a CType.
cTypeLlvm :: CType -> T.Text
cTypeLlvm CI8  = "i8"
cTypeLlvm CI16 = "i16"
cTypeLlvm CI32 = "i32"
cTypeLlvm CI64 = "i64"
cTypeLlvm CU8  = "i8"
cTypeLlvm CU16 = "i16"
cTypeLlvm CU32 = "i32"
cTypeLlvm CU64 = "i64"
cTypeLlvm CF32 = "float"
cTypeLlvm CF64 = "double"
cTypeLlvm CPtr = "ptr"
cTypeLlvm CVoid = "void"
cTypeLlvm (CArr _ _) = "ptr"   -- arrays accessed as pointers
cTypeLlvm (CStruct _) = "ptr"  -- nested structs stored as pointers

-- C struct layout computation: size, alignment, and field offsets.

cTypeSize :: CType -> Int
cTypeSize CI8  = 1
cTypeSize CI16 = 2
cTypeSize CI32 = 4
cTypeSize CI64 = 8
cTypeSize CU8  = 1
cTypeSize CU16 = 2
cTypeSize CU32 = 4
cTypeSize CU64 = 8
cTypeSize CF32 = 4
cTypeSize CF64 = 8
cTypeSize CPtr = 8
cTypeSize CVoid = 0
cTypeSize (CArr n ct) = n * cTypeSize ct
cTypeSize (CStruct _) = 8  -- fallback; use cTypeSizeWith for nested structs

cTypeAlign :: CType -> Int
cTypeAlign (CArr _ ct) = cTypeAlign ct  -- array alignment = element alignment
cTypeAlign (CStruct _) = 8  -- fallback; use cTypeAlignWith for nested structs
cTypeAlign ct = cTypeSize ct  -- on x86-64, alignment == size for all primitive types

-- Size/alignment with struct layout lookup
cTypeSizeWith :: M.Map T.Text StructLayout -> CType -> Int
cTypeSizeWith m (CStruct name) = case M.lookup name m of
  Just sl -> slSize sl
  Nothing -> 8
cTypeSizeWith _ ct = cTypeSize ct

cTypeAlignWith :: M.Map T.Text StructLayout -> CType -> Int
cTypeAlignWith m (CStruct name) = case M.lookup name m of
  Just sl -> if slSize sl == 0 then 1 else maximum [cTypeAlignWith m ct | (_, _, ct) <- slOffsets sl]
  Nothing -> 8
cTypeAlignWith _ (CArr _ ct) = cTypeAlign ct
cTypeAlignWith _ ct = cTypeSize ct

data StructLayout = StructLayout
  { slSize    :: !Int           -- total struct size (padded to alignment)
  , slOffsets :: [(T.Text, Int, CType)]  -- (field name, byte offset, field CType)
  } deriving (Show)

type StructMap = M.Map T.Text StructLayout

computeStructLayout :: [(T.Text, CType)] -> StructLayout
computeStructLayout = computeStructLayoutWith M.empty

computeStructLayoutWith :: StructMap -> [(T.Text, CType)] -> StructLayout
computeStructLayoutWith m fields =
  let (offsets, endOffset) = go 0 fields
      maxAlign = if null fields then 1 else maximum (map (cTypeAlignWith m . snd) fields)
      totalSize = alignUp endOffset maxAlign
  in StructLayout totalSize offsets
  where
    go offset [] = ([], offset)
    go offset ((name, ct):rest) =
      let a = cTypeAlignWith m ct
          aligned = alignUp offset a
          s = cTypeSizeWith m ct
          (rest', endOff) = go (aligned + s) rest
      in ((name, aligned, ct) : rest', endOff)
    alignUp n a = ((n + a - 1) `div` a) * a

renderType :: Type -> T.Text
renderType t = case t of
  TyInt      -> "%INT"
  TyFlt      -> "%FLT"
  TyStr      -> "%STR"
  TyBool     -> "%BOOL"
  TyUnit     -> "%UNIT"
  TyRx    -> "%RX"
  TyUSym  -> "%USYM"
  TySyntax -> "%SYNTAX"
  TyFun as r -> "%(-> " <> T.intercalate " " (map renderType (as ++ [r])) <> ")"
  TyCon s [] -> "%" <> s
  TyCon s ts -> "%(" <> s <> " " <> T.intercalate " " (map renderType ts) <> ")"
  TyApp f a  -> "%(" <> renderType f <> " " <> renderType a <> ")"
  TyVar n    -> "%t" <> T.pack (show n)
