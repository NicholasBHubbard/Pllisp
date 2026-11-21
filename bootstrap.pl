#!/usr/bin/perl

# This is the bootstrap compiler for pllisp

use strict;
use warnings;
use v5.36;

use autodie;

use Data::Dumper;

exit main(@ARGV);

sub main {
    my ($file) = @_;
    open(my $fh, '<', $file);
    my $program = do { local $/; <$fh> };
    close $fh;

    my @tokens = Lexer->new($program)->lex();
    say 'LEX RESULT:';
    print Dumper(\@tokens);

    my $ast = Parser->new(@tokens)->parse();
    say "\nPARSE RESULT:";
    print Dumper($ast);

    return 0;
}

package Lexer {

sub new {
    my ($class, $program) = @_;
    return bless {
        program => $program,
    }, $class;
}

sub lex {
    my ($self) = @_;

    my @token_table = (
        { TYPE => 'TOK_LPAREN',     RE => qr/\(/                         },
        { TYPE => 'TOK_RPAREN',     RE => qr/\)/                         },
        { TYPE => 'TOK_ARROW',      RE => qr/->/                         },
        { TYPE => 'TOK_COLON',      RE => qr/:/                          },
        { TYPE => 'TOK_STRING',     RE => qr/"([^"\\\r\n]|\\["\\nrt])*"/ },
        { TYPE => 'TOK_FLOAT',      RE => qr/[0-9]+\.[0-9]+/             },
        { TYPE => 'TOK_INTEGER',    RE => qr/[0-9]+/                     },
        { TYPE => 'TOK_KW_FUN',     RE => qr/fun\b/                      },
        { TYPE => 'TOK_KW_LET',     RE => qr/let\b/                      },
        { TYPE => 'TOK_KW_MATCH',   RE => qr/match\b/                    },
        { TYPE => 'TOK_KW_TYPE',    RE => qr/type\b/                     },
        { TYPE => 'TOK_IDENTIFIER', RE => qr/[a-z_][a-zA-Z0-9_]*/        },
        { TYPE => 'TOK_TYPENAME',   RE => qr/[A-Z][a-zA-Z0-9_]*/         },
    );

    pos($self->{program}) = 0;
    my @tokens;
  TOKEN: while (pos $self->{program} < length $self->{program}) {
        next TOKEN if $self->{program} =~ /\G(?:[ \t\r\n]+|#[^\r\n]*)/gc;

        for my $rule (@token_table) {
            next unless $self->{program} =~ /\G($rule->{RE})/gc;

            push @tokens, {
                TYPE  => $rule->{TYPE},
                VAL   => $1,
                SPAN  => { START => $-[0], END => $+[0] },
                TOKEN => 1,
            };

            next TOKEN;
        }

        die "lex error at offset ", pos($self->{program}), "\n";
    }

    push @tokens, {
        TYPE  => 'TOK_EOF',
        VAL   => undef,
        SPAN  => { START => length $self->{program}, END => length $self->{program} },
        TOKEN => 1,
    };

    return @tokens;
}

} # package Lexer

package Parser {

sub new {
    my ($class, @tokens) = @_;
    return bless {
        tokens => \@tokens,
        cursor => 0,
    }, $class;
}

sub parse {
    my ($self) = @_;
    my @forms;
    push @forms, $self->_p_form() until $self->_accept('TOK_EOF');
    return \@forms;
}

sub _p_form {
    my ($self) = @_;

    return $self->_p_atom() unless $self->_accept('TOK_LPAREN');

    my $node;
    if ($self->_peek('TOK_KW_TYPE')) {
        $node = $self->_p_type_declaration();
    } elsif ($self->_peek('TOK_KW_FUN')) {
        $node = $self->_p_fun();
    } elsif ($self->_peek([qw(TOK_KW_LET TOK_KW_MATCH TOK_IDENTIFIER TOK_TYPENAME)])) {
        $node = $self->_p_common_form();
    } else {
        $self->_error("expected 'type', 'fun', 'let', 'match', or a callee");
    }
    $self->_expect('TOK_RPAREN');

    return $node;
}

sub _p_expression {
    my ($self) = @_;

    return $self->_p_atom() unless $self->_accept('TOK_LPAREN');

    my $node;
    if ($self->_peek('TOK_KW_FUN')) {
        $node = $self->_p_fun_anonymous();
    } elsif ($self->_peek([qw(TOK_KW_LET TOK_KW_MATCH TOK_IDENTIFIER TOK_TYPENAME)])) {
        $node = $self->_p_common_form();
    } else {
        $self->_error("expected 'fun', 'let', 'match', or a callee");
    }

    $self->_expect('TOK_RPAREN');
    return $node;
}

sub _p_type_declaration {
    my ($self) = @_;

    $self->_expect('TOK_KW_TYPE');

    my $has_params = $self->_accept('TOK_LPAREN');
    my $name = $self->_expect('TOK_TYPENAME');
    my @params;

    if ($has_params) {
        do {
            push @params, $self->_expect('TOK_IDENTIFIER');
        } until ($self->_accept('TOK_RPAREN'));
    }

    my @variants;
    do {
        $self->_expect('TOK_LPAREN');
        my $constructor = $self->_expect('TOK_TYPENAME');

        my @fields;
        until ($self->_accept('TOK_RPAREN')) {
            push @fields, $self->_p_type_expression();
        }

        push @variants, { NAME => $constructor, FIELDS => \@fields };
    } while ($self->_peek('TOK_LPAREN'));

    return {
        TYPE_DECLARATION => {
            NAME            => $name,
            TYPE_PARAMETERS => \@params,
            VARIANTS        => \@variants,
        },
    };
}

sub _p_type_expression {
    my ($self) = @_;

    if (my $name = $self->_accept('TOK_TYPENAME')) {
        return { TYPE_NAME => $name };
    } elsif (my $var = $self->_accept('TOK_IDENTIFIER')) {
        return { TYPE_VARIABLE => $var };
    } elsif ($self->_accept('TOK_LPAREN')) {
        my $name = $self->_expect('TOK_TYPENAME');
        my @args;
        do {
            push @args, $self->_p_type_expression();
        } until ($self->_accept('TOK_RPAREN'));

        return { TYPE_APPLICATION => { NAME => $name, ARGS => \@args } };
    } else {
        $self->_error('expected a type');
    }
}

sub _p_common_form {
    my ($self) = @_;

    if ($self->_peek('TOK_KW_LET')) {
        return $self->_p_let();
    } elsif ($self->_peek('TOK_KW_MATCH')) {
        return $self->_p_match();
    } elsif ($self->_peek([qw(TOK_IDENTIFIER TOK_TYPENAME)])) {
        return $self->_p_call();
    } else {
        $self->_error("expected 'let', 'match', or a callee");
    }
}

sub _p_let {
    my ($self) = @_;

    $self->_expect('TOK_KW_LET');
    $self->_expect('TOK_LPAREN');

    my $name = $self->_expect('TOK_IDENTIFIER');
    my $type; $type = $self->_p_type_expression() if $self->_accept('TOK_COLON');

    my $value = $self->_p_expression();
    $self->_expect('TOK_RPAREN');

    my $body = $self->_p_expression();
    return {
        LET => {
            BINDING => { NAME => $name, TYPE => $type, VALUE => $value },
            BODY    => $body,
        },
    };
}

sub _p_match {
    my ($self) = @_;

    $self->_expect('TOK_KW_MATCH');
    my $subject = $self->_p_expression();

    my @arms;
    do {
        $self->_expect('TOK_LPAREN'); # Start of arm
        $self->_expect('TOK_LPAREN'); # Start of pattern

        my $constructor = $self->_expect('TOK_TYPENAME');
        my @bindings;
        while (my $name = $self->_accept('TOK_IDENTIFIER')) {
            push @bindings, $name;
        }

        $self->_expect('TOK_RPAREN'); # End of pattern
        my $body = $self->_p_expression();
        $self->_expect('TOK_RPAREN'); # End of arm

        push @arms, {
            PATTERN => { CONSTRUCTOR => $constructor, BINDINGS => \@bindings },
            BODY    => $body,
        };
    } while ($self->_peek('TOK_LPAREN'));

    return { MATCH => { SUBJECT => $subject, ARMS => \@arms } };
}

sub _p_call {
    my ($self) = @_;

    my $callee = $self->_expect([qw(TOK_IDENTIFIER TOK_TYPENAME)]);
    my @args;

    until ($self->_peek('TOK_RPAREN')) {
        push @args, $self->_p_expression();
    }

    return { CALL => { CALLEE => $callee, ARGS => \@args } };
}

sub _p_fun {
    my ($self) = @_;

    if ($self->_peek('TOK_KW_FUN', 'TOK_LPAREN')) {
        return $self->_p_fun_anonymous();
    }

    $self->_expect('TOK_KW_FUN');

    if (my $name = $self->_accept('TOK_IDENTIFIER')) {
        my $fun = $self->_p_fun_tail();
        $fun->{NAME} = $name;
        return { FUN_NAMED => $fun };
    } else {
        $self->_error("expected a function name or '('");
    }
}

sub _p_fun_anonymous {
    my ($self) = @_;
    $self->_expect('TOK_KW_FUN');
    return { FUN_ANONYMOUS => $self->_p_fun_tail() };
}

sub _p_fun_tail {
    my ($self) = @_;

    my $parameters = $self->_p_lambda_list();

    my $type; if ($self->_accept('TOK_ARROW')) {
        $type = $self->_p_type_expression();
    }

    my @body = ($self->_p_expression());
    until ($self->_peek('TOK_RPAREN')) {
        push @body, $self->_p_expression();
    }

    return {
        PARAMETERS => $parameters,
        RETURN_TYPE => $type,
        BODY => \@body,
    };
}

sub _p_lambda_list {
    my ($self) = @_;

    $self->_expect('TOK_LPAREN');

    my @parameters;
    until ($self->_accept('TOK_RPAREN')) {
        my $identifier = $self->_expect('TOK_IDENTIFIER');
        my $type; if ($self->_accept('TOK_COLON')) {
            $type = $self->_p_type_expression();
        }
        push @parameters, { NAME => $identifier, TYPE => $type, };
    }

    return \@parameters;
}

sub _p_atom {
    my ($self) = @_;
    if (my $token = $self->_accept([qw(TOK_IDENTIFIER TOK_INTEGER TOK_FLOAT TOK_STRING)])) {
        return { ATOM => $token };
    } else {
        $self->_error();
    }
}

sub _accept {
    my ($self, @types) = @_;
    if (my $match = $self->_peek(@types)) {
        $self->{cursor} += (@types || 1);
        return $match;
    }
    return 0;
}

sub _expect {
    my ($self, @types) = @_;
    if (my $match = $self->_accept(@types)) {
        return $match;
    }
    $self->_error(do {
        my @parts = map { ref($_) eq 'ARRAY' ? join(' or ', @$_) : $_ } @types;
        @parts ? 'expected ' . join(' then ', @parts) : 'expected a token';
    });
}

sub _peek {
    my ($self, @types) = @_;

    return $self->{tokens}[$self->{cursor}] unless @types;

    my @tokens;
    for my $i (0 .. $#types) {
        my $token = $self->{tokens}[ $self->{cursor} + $i ] // return 0;
        my $expected = $types[$i];

        if (ref $expected eq 'ARRAY') {
            return 0 unless grep { $_ eq $token->{TYPE} } @$expected;
        } else {
            return 0 unless $expected eq $token->{TYPE};
        }

        push @tokens, $token;
    }

    return @tokens == 1 ? $tokens[0] : \@tokens;
}

sub _error {
    my ($self, $msg) = @_;
    my $token = $self->_peek();

    my $error = "unexpected $token->{TYPE} at offset $token->{SPAN}{START}";
    $error .= " ($msg)" if defined $msg;

    for (my $i = 1; my @frame = caller($i); $i++) {
        die "$error while parsing @{[uc $1]}\n" if $frame[3] =~ /::_p_(\w+)$/;
    }
    die "$error\n";
}

} # package Parser

package Evaluator {

sub new {
    my ($class, $ast) = @_;
    return bless {
        ast => $ast,
        env => { symtab => [] },
    }, $class;
}

# each scope gets a symbol table

# sub eval {
#     my ($self) = @_;
# }

# sub _e_let {
#     my ($self) = @_;
# }

} # package Evaluator

__END__
