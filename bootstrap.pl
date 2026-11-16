#!/usr/bin/perl

# This is the bootstrap compiler for pllisp

use strict;
use warnings;
use v5.36;

use autodie;

sub main {
    my ($file) = @_;
    open(my $fh, '<', $file);
    my $program = do { local $/; <$fh> };
    my @tokens = lex($program);
    say $_->{TYPE}, ': ', $_->{VAL}, ': ', $_->{SPAN}{START}, ',', $_->{SPAN}{END} for @tokens;
    return 0;
}

sub lex {
    my ($program) = @_;

    my @token_table = (
        { TYPE => 'LPAREN',     RE => qr/\(/                  },
        { TYPE => 'RPAREN',     RE => qr/\)/                  },
        { TYPE => 'STRING',     RE => qr/"[^"]*"/             },
        { TYPE => 'FLOAT',      RE => qr/[0-9]+\.[0-9]+/      },
        { TYPE => 'INTEGER',    RE => qr/[0-9]+/              },
        { TYPE => 'FUN',        RE => qr/fun/                 },
        { TYPE => 'TYPE',       RE => qr/type/                },
        { TYPE => 'LET',        RE => qr/let/                 },
        { TYPE => 'IDENTIFIER', RE => qr/[a-z_][a-zA-Z0-9_]*/ },
        { TYPE => 'TYPENAME',   RE => qr/[A-Z][a-zA-Z0-9_]*/  },
    );

    my @tokens;
    while (pos $program < length $program) {
        for my $rule (@token_table) {
            if ($program =~ m/\G[ \t\n\r]*($rule->{RE})[ \t\n\r]*/gc) {
                push @tokens, {
                    TYPE => $rule->{TYPE},
                    VAL  => $1,
                    SPAN => { START => $-[0], END => $-[0] + length $1 },
                };
                last;
            }
        }
    }

    return @tokens;
}

sub parse {
    my @tokens = @_;
    my %ast = _parse_program(@tokens);
    return %ast;
}

sub _parse_program {
    _parse_c_zero_or_more(&_parse_expression);
}

sub _parse_c_peeksym {
    return $TOKENS[0];
}

sub _parse_c_consumesym {
    return shift @TOKENS;
}

sub _parse_c_error {
    $! = 1;
    die "parse error: @_\n";
}


exit main(@ARGV);

__END__
