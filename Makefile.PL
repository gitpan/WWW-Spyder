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
        'version'    => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'WWW-Spyder-*' },
);
