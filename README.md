# WiLiKi

This is WiLiKi, a Wiki engine written in [Gauche Scheme](https://practical-scheme.net/gauche).

You can visit the [official site](https://practical-scheme.net/wiliki/wiliki.cgi) for the details.

## Prerequisites

You need Gauche installed on your machine.
See [`package.scm`](package.scm) for the required minimum version.

WiLiKi is supposed to run as a CGI script, so you need a web server
setup that can run CGI scripts.

Alternatively, you can install [Gauche-makiki](https://github.com/shirok/Gauche-makiki) webserver to run WiLiKi stand-alone.


## Installation

Clone the repo, and run configure & make.

```
./configure
make
make -s check
make install
```

`make install` installs WiLiKi as a Gauche library.

## Running

To run the wiki, you need to copy [`wiliki.cgi`](src/wiliki.cgi)
to your site's cgi script location and edit it to suit your
site's setup.

To run the wiki in stand-alone mode,
run `src/wiliki-server` script (you still need to prepare cgi
script with your own customization.)


```
wiliki-server -p <port> <your-customized-wiliki.cgi>
```
