#!/usr/bin/perl -0777 -p
BEGIN { $np = qr{(?:(?>[^()]+)|\((??{$np})\))*} } # from perlre man page
s/\bweight\s*\($np,\s*($np)\s*\)/$1/s;
