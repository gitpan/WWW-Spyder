use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'WWW::Spyder',
    AUTHOR              => 'Ashley Pond V <ashley@cpan.org>',
    VERSION_FROM        => 'lib/WWW/Spyder.pm',
    ABSTRACT_FROM       => 'lib/WWW/Spyder.pm',
    PL_FILES            => {},
    PREREQ_PM => {
        'Test::More' => 0,
# NOT YET TOO MUCH TO FIX 'Test::Perl::Critic'=> '0',
        "HTML::Parser" => 3,
        "LWP::UserAgent" => 0,
        "HTTP::Cookies" => 0,
        "URI::URL" => 0,
        "HTML::Entities" => 0,
        "Digest::MD5" => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'WWW-Spyder-*' },
);
