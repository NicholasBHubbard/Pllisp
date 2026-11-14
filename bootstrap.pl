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
    say $_->{TYPE}, ': ', $_->{VAL} for @tokens;
    return 0;
}

sub lex {
    my ($program) = @_;

    my %token_rules = (
        LPAREN     => qr/\(/,
        RPAREN     => qr/\)/,
        SYMBOL     => qr/[a-zA-Z][a-zA-Z0-9_]*/,
        STRING     => qr/".*"/,
        FLOAT      => qr/[0-9]+\.[0-9]+/,
        INTEGER    => qr/[0-9]+/,
    );

    my @tokens;
    while (pos $program < length $program) {
        for my $rule (sort keys %token_rules) {
            if ($program =~ m/\G\s*($token_rules{$rule})\s*/gcm) {
                push @tokens, {TYPE => $rule, VAL => $1};
                last;
            }
        }
    }
    return @tokens;
}

sub parse {
    my @tokens = @_;

    my %derivations = ();
}

exit main(@ARGV);

__END__
