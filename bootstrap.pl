#!/usr/bin/perl

# This is the bootstrap compiler for pllisp

use strict;
use warnings;
use v5.36;

use autodie;

use Data::Dumper;

sub main {
    my ($file) = @_;
    open(my $fh, '<', $file);
    my $program = do { local $/; <$fh> };
    close $fh;

    my @tokens = lex($program);
    say 'LEX RESULT:';
    print Dumper(\@tokens);

    my $ast = Parser->new(@tokens)->parse();
    say "\nPARSE RESULT:";
    print Dumper($ast);

    return 0;
}

sub lex {
    my ($program) = @_;

    my @token_table = (
        { TYPE => 'TOK_LPAREN',     RE => qr/\(/                         },
        { TYPE => 'TOK_RPAREN',     RE => qr/\)/                         },
        { TYPE => 'TOK_ARROW',      RE => qr/->/                         },
        { TYPE => 'TOK_COLON',      RE => qr/:/                          },
        { TYPE => 'TOK_STRING',     RE => qr/"([^"\\\r\n]|\\["\\nrt])*"/ },
        { TYPE => 'TOK_FLOAT',      RE => qr/[0-9]+\.[0-9]+/             },
        { TYPE => 'TOK_INTEGER',    RE => qr/[0-9]+/                     },
        { TYPE => 'TOK_KW_FUN',     RE => qr/fun/                        },
        { TYPE => 'TOK_KW_LET',     RE => qr/let/                        },
        { TYPE => 'TOK_KW_MATCH',   RE => qr/match/                      },
        { TYPE => 'TOK_KW_TYPE',    RE => qr/type/                       },
        { TYPE => 'TOK_IDENTIFIER', RE => qr/[a-z_][a-zA-Z0-9_]*/        },
        { TYPE => 'TOK_TYPENAME',   RE => qr/[A-Z][a-zA-Z0-9_]*/         },
    );

    my @tokens;
    for (pos($program) = 0; pos $program < length $program;) {
        for my $rule (@token_table) {
            next if $program =~ /\G(?:[ \t\r\n]+|#[^\r\n]*)/gc;
            if ($program =~ /\G($rule->{RE})/gc) {
                push @tokens, {
                    TYPE => $rule->{TYPE},
                    VAL  => $1 // '',
                    SPAN => { START => $-[0], END => $-[0] + length $1 },
                };
                last;
            }
        }
    }
    push @tokens, {
        TYPE => 'TOK_EOF',
        VAL  => undef,
        SPAN => { SPAN => { START => length $program, END => length $program } },
    };

    return @tokens;
}

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
    my @forms = $self->_p_program();
    $self->_expect('TOK_EOF');
    return \@forms;
}

sub _p_program {
    my ($self) = @_;
    my @forms;
    while (not $self->_peek('TOK_EOF')) {
        push @forms, $self->_p_form();
    }
    return @forms;
}

sub _p_atom {
    my ($self) = @_;

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
        return $match
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

exit main(@ARGV);

__END__
