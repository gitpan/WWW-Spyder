use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile
    (
     'NAME'	     => 'WWW::Spyder',
     'VERSION_FROM'  => 'Spyder.pm', # finds $VERSION
     'PREREQ_PM'     => { 'HTML::Parser'    => 3,
                          'LWP::UserAgent'  => 1.7,
                          'HTTP::Cookies'   => 1.14,
                          'URI::URL'        => 5,
                          'HTML::Entities'  => 1.2,
                          'Digest::MD5'     => 2.14,
                      },
     'AUTHOR'        => 'Ashley Pond V <ashley@sedition.com>',
     'ABSTRACT'      => 'WWW::Spyder creates web spiders with ' .
                           'hooks into text, html, links and ' .
                               'link names.',
     );
