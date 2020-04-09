package SExpression::Decode::Regexp;

# AUTHORITY
# DATE
# DIST
# VERSION

use 5.010001;
use strict;
use warnings;

#use Data::Dumper;

require Exporter;
our @ISA       = qw(Exporter);
our @EXPORT_OK = qw(from_sexp);

sub _fail {
    my $snippet = substr($_, pos(), 11);
    if (length $snippet == 11) { $snippet = substr($snippet, 0, 10) . "..." }
    die __PACKAGE__.": $_[0] at offset ".pos()." near '$snippet'\n";
}

sub _decode_char_escaped_1char {
    my $char = substr($_[0], -1, 1);
    if    ($char eq 'a') { return chr(7) }
    elsif ($char eq 'b') { return chr(8) }
    elsif ($char eq 't') { return chr(9) }
    elsif ($char eq 'n') { return chr(10) }
    elsif ($char eq 'v') { return chr(11) }
    elsif ($char eq 'f') { return chr(12) }
    elsif ($char eq 'r') { return chr(13) }
    elsif ($char eq 'e') { return chr(27) }
    elsif ($char eq 's') { return chr(32) }
    elsif ($char eq 'd') { return chr(127) }
    else { return $char }
}

sub _decode_char_escaped_control {
    my $char = lc substr($_[0], -1, 1);
    return chr(ord($char) - 97+1);
}

sub _decode_int_hex {
    my $str = shift;
    $str =~ s/\A#[xX]//;
    $str =~ s/\A([+-]?)//;
    my $sign = $1 // '';
    ($sign eq '-' ? -1:1) * hex($str);
}

sub _decode_inf_nan {
    my $str = shift;
    $str =~ s/\A([+-]?)//;
    my $sign = $1 // '';
    $str =~ /NaN/ ? "NaN" : ($sign eq '-' ? -1:1) * "Inf";
}

sub _decode_int_oct {
    my $str = shift;
    $str =~ s/\A#[oO]//;
    $str =~ s/\A([+-]?)//;
    my $sign = $1 // '';
    ($sign eq '-' ? -1:1) * oct($str);
}

sub _decode_int_bin {
    my $str = shift;
    $str =~ s/\A#[bB]//;
    $str =~ s/\A([+-]?)//;
    my $sign = $1 // '';
    ($sign eq '-' ? -1:1) * oct("0b$str");
}

sub _decode_int_radix {
    require Math::NumberBase;

    my $str = shift;
    my ($base, $num) = $str =~ /\A#([0-9]+)[rR](\w+)/ or die;
    $str =~ s/\A([+-]?)//;
    my $sign = $1 // '';
    ($sign eq '-' ? -1:1) * Math::NumberBase->new($base)->to_decimal(lc $num);
}

our $FROM_SEXP = qr{

(?:
    (?&VALUE) (?{ $_ = $^R->[1] })
|
    \z (?{ _fail "Unexpected end of input" })
|
      (?{ _fail "Invalid literal" })
)

(?(DEFINE)

(?<VALUE>
  (
      (?&NUMBER_INT_RADIX)
  |
      (?&NUMBER_INT_OCT)
  |
      (?&NUMBER_INT_HEX)
  |
      (?&NUMBER_INT_BIN)
  |
      (?&NUMBER_INF_NAN)
  |
      (?&NUMBER_FLOAT)
  |
      (?&CHAR_ESCAPED_CONTROL)
  |
      (?&CHAR_ESCAPED_1CHAR)
  |
      (?&CHAR_UNESCAPED)
  |
      (?&STRING)
  |
      (?&PAIR)
  |
      (?&LIST)
  |
      (?&VECTOR)
  |
      (?&ATOM)
  )
)

(?<NUMBER_FLOAT>
  (
    [+-]?
    (?: [0-9]+\.?[0-9]* | \.[0-9]+ )
    (?: [eE] [-+]? [0-9]+ )?
  )

  (?{ [$^R, 0+$^N] })
)

(?<NUMBER_INF_NAN>
  (
    [+-]?
    (?: [0-9]+ | [0-9]*\.[0-9]+ )
    (?: [eE] \+ (?:INF|NaN) )
  )

  (?{ [$^R, _decode_inf_nan($^N)] })
)

(?<NUMBER_INT_HEX>
  (
    \#[xX]
    [+-]?
    [0-9A-Fa-f]+
  )

  (?{ [$^R, _decode_int_hex($^N)] })
)

(?<NUMBER_INT_OCT>
  (
    \#[oO]
    [+-]?
    [0-7]+
  )

  (?{ [$^R, _decode_int_oct($^N)] })
)

(?<NUMBER_INT_BIN>
  (
    \#[bB]
    [+-]?
    [0-1]+
  )

  (?{ [$^R, _decode_int_bin($^N)] })
)

(?<NUMBER_INT_RADIX>
  (
    \#[1-9][0-9]*[rR]
    [+-]?
    [0-9A-Za-z]+
  )

  (?{ [$^R, _decode_int_radix($^N)] })
)

(?<CHAR_UNESCAPED>
  (
      \?[^\\\(]
  )

  (?{ [$^R, ord(substr($^N, 1, 1))] })
)

(?<CHAR_ESCAPED_1CHAR>
  (
      \?\\[^\^C]
  )

  (?{ [$^R, _decode_char_escaped_1char($^N)] })
)

(?<CHAR_ESCAPED_CONTROL>
  (
      \?\\(?:C-|\^)[A-Za-z]
  )

  (?{ [$^R, _decode_char_escaped_control($^N)] })
)

(?<STRING>
  "
  (?{ [$^R, ""] })
  (
    (?:
      ([^\\"]+) (?{ [$^R->[0], $^R->[1] . $^N] })
    |
      (\\(?:C-|\^)[A-Za-z]) (?{ [$^R->[0], $^R->[1] . _decode_char_escaped_control($^N)] })
    |
      (\\[^\^C]) (?{ [$^R->[0], $^R->[1] . _decode_char_escaped_1char($^N)] })
    |
        # XXX support \u...., \N{...}
      (?: [^"]) (?{ _fail "Invalid escape sequence in string literal" })
    )*
  )
  (?:
    "
  |
    (?:\\|\z) (?{ _fail "Expected closing of string" })
  )
)

(?<PAIR>
  \(\s*
  (?{ [$^R, []] })
  (?&VALUE) # [[$^R, []], $val]
  (?{ [$^R->[0][0], [$^R->[1]]] })
  \s+ \. \s+
  (?&VALUE)
  (?{ push @{$^R->[0][1]}, $^R->[1]; $^R->[0] })
  \s*
  (?:
      \)
  |
      (?:.|\z) (?{ _fail "Expected closing of pair" })
  )
)

(?<LIST>
  \(\s*
  (?{ [$^R, []] })
  (?:
      (?&VALUE) # [[$^R, []], $val]
      (?{ [$^R->[0][0], [$^R->[1]]] })
      (?:
          (?:
              \s+ (?&VALUE)
              (?{ push @{$^R->[0][1]}, $^R->[1]; $^R->[0] })
          )*
      |
          (?: [^\)]|\z ) (?{ _fail "Expected '\x41'" })
      )
  )?
  \s*
  (?:
      \)
  |
      (?:.|\z) (?{ _fail "Expected closing of list" })
  )
)

(?<VECTOR>
  \[\s*
  (?{ [$^R, []] })
  (?:
      (?&VALUE) # [[$^R, []], $val]
      (?{ [$^R->[0][0], [$^R->[1]]] })
      (?:
          (?:
              \s+ (?&VALUE)
              (?{ push @{$^R->[0][1]}, $^R->[1]; $^R->[0] })
          )*
      |
          (?: [^\]]|\z ) (?{ _fail "Expected '\x5d'" })
      )
  )?
  \s*
  (?:
      \]
  |
      (?:.|\z) (?{ _fail "Expected closing of vector" })
  )
)

(?<ATOM>
  ([^\s\\()]+) # XXX not quite correct
  (?{ [$^R, $^N] })
)

# XXX support #("char" property-data) https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-Props-and-Strings.html#Text-Props-and-Strings

) }xms;

sub from_sexp {
    state $re = qr{\A$FROM_SEXP\z};

    local $_ = shift;
    s/\A\s+//s; s/\s+\z//s;
    local $^R;
    eval { $_ =~ $re } and return $_;
    die $@ if $@;
    die 'no match';
}

1;
# ABSTRACT: S-expression parser as a single Perl Regex

=head1 SYNOPSIS

 use SExpression::Decode::Regexp qw(from_sexp);
 my $data = from_sexp(q|(setq foo '(1 2 3))|);

 use Data::Dump;
 dd $data;

will print:

 ["setq", "foo", [1, 2, 3]]


=head1 DESCRIPTION


=head1 FUNCTIONS

=head2 from_sexp

Usage:

 my $data = from_sexp($str);

Decode S-expression in C<$str>. Dies on error.


=head1 FAQ


=head1 SEE ALSO

=head2 Other modules to parse S-expression

L<Data::SExpression>

=head2 Tangentially related

L<JSON::Decode::Regexp>

=cut
