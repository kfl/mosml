Distribution: IT University of Copenhagen
Vendor: IT University of Copenhagen
Packager: Jakob Lichtenberg <jl@itu.dk>
URL: http://www.dina.kvl.dk/~sestoft/mosml.html
Summary:  Moscow ML
Name: mosml
Version: 2.00
Release: 1
Source: ftp://ftp.dina.kvl.dk/pub/mosml/mos20src.tar.gz 
Patch: mosml_dynlibs_setup.patch
Patch1: mosml_bindir.patch
Patch2: mosml_ccc.patch
Copyright: GPL
Group: Development/Languages
BuildRoot: /tmp/%{name}-%{version}-root
%description
Moscow ML provides a light-weight implementation of full Standard ML,
including Modules and some extensions.  Standard ML is a strict
functional language widely used in teaching and research.

Moscow ML is based on the Caml Light system, which gives fast
compilation and modest storage consumption.

   * The full SML Modules language (structures, signatures, and functors) 
     is now supported, thanks to Claudio Russo.  
   * Also, several extensions to the SML Modules language are provided:
      - higher-order functors: functors may be defined within structures 
        and functors
      - first-class modules: structures and functors may be packed and 
        then handled as Core language values, which may then be unpacked 
        as structures or functors again
      - recursive modules: signatures and structures may be recursively 
        defined
   * Despite that improvements, Moscow ML remains backwards compatible.
   * Value polymorphism has become friendlier: non-generalizable free
     type variables are left free, and become instantiated (once only)
     when the bound variable is used
   * Added facilities for creating and communicating with subprocesses
     (structure Unix and Signal from SML Basis Library).
   * Added facilities for efficient functional generation of HTML code
     (structure Msp); also supports the writing of ML Server Page scripts.
   * Added facilities setting and accessing `cookies' in CGI scripts
     (structure Mosmlcookie), thanks to Hans Molin, Uppsala, Sweden.
   * The Gdimage structure now produces PNG images (using Thomas 
     Boutell's gd library).


Moscow ML version 2.00 is available from

              http://www.dina.kvl.dk/~sestoft/mosml.html


This RPM contains the following dynlibs:
  * Gdbm
  * Mysql
  * Postgres
  * Regex
  * Socket
  * Unix

The following dynlibs are not build:
  * Gdimage
  * IntInf
  * interface
  * crypt


%prep
%setup -n mosml
%patch
%patch1
# Comment in to use Alpha Compaq C Compiler, ccc:
# %patch2 


%build
cd src
make MOSMLHOME=${RPM_BUILD_ROOT}/usr/mosml world


%install
rm -rf ${RPM_BUILD_ROOT}
cd src
make MOSMLHOME=${RPM_BUILD_ROOT}/usr/mosml install
cd dynlibs
make MOSMLHOME=${RPM_BUILD_ROOT}/usr/mosml 
make MOSMLHOME=${RPM_BUILD_ROOT}/usr/mosml install
cd ..
cd ..

cp -a tools/Makefile.stub ${RPM_BUILD_ROOT}/usr/mosml/tools

# Put installed doc in an rpm manner
cp -a ${RPM_BUILD_ROOT}/usr/mosml/doc .
rm -rf ${RPM_BUILD_ROOT}/usr/mosml/doc

# Link binaries to the standard search path /usr/bin
mkdir -p ${RPM_BUILD_ROOT}/usr/bin
cd ${RPM_BUILD_ROOT}/usr/mosml/bin
for x in * 
do
  ln -sf ../mosml/bin/$x ${RPM_BUILD_ROOT}/usr/bin/$x
done

# Alternatively, you can move bin out of mosml:
# mkdir -p ${RPM_BUILD_ROOT}/usr
# mv ${RPM_BUILD_ROOT}/usr/mosml/bin ${RPM_BUILD_ROOT}/usr/bin

# dynamic load path is extended to include /usr/mosml/lib,
# Instead you can move libraries to the standard dynamic loadpath /usr/lib
# mkdir -p ${RPM_BUILD_ROOT}/usr/lib
# cd ${RPM_BUILD_ROOT}/usr/mosml/lib
# for x in lib*.so 
# do
#   mv ${RPM_BUILD_ROOT}/usr/mosml/lib/${x} ${RPM_BUILD_ROOT}/usr/lib/${x}
#   # to link instead: ln -sf ../mosml/lib/${x}  ${RPM_BUILD_ROOT}/usr/lib/${x}
# done

# Fix minor install problem:
rm -f ${RPM_BUILD_ROOT}/usr/mosml/lib/camlrunm
ln -s ../bin/camlrunm ${RPM_BUILD_ROOT}/usr/mosml/lib/camlrunm

%clean
rm -rf ${RPM_BUILD_ROOT}

%post
if ! grep '^/usr/mosml/lib$' /etc/ld.so.conf > /dev/null 2>&1; then
  echo "/usr/mosml/lib" >> /etc/ld.so.conf
fi
/sbin/ldconfig

%postun
if [ "$1" = "0" ]; then
  grep -v '^/usr/mosml/lib$' /etc/ld.so.conf > /etc/ld.so.conf.new 2>/dev/null
  cat /etc/ld.so.conf.new > /etc/ld.so.conf
  rm -f /etc/ld.so.conf.new
fi
/sbin/ldconfig

%files
%defattr(-,root,root)
/usr/mosml
/usr/bin/*
%doc README copyrght doc/* examples

%changelog

* Fri Jun 30 2000 Jakob Lichtenberg <jl@itu.dk>
- initial release
