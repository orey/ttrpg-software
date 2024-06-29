@echo off
set NODEPATH=C:\ProgramData\orey\Software\node-v16.13.0-win-x64\
set PATH=%NODEPATH%;%PATH%

set proxy=http://127.0.0.1:3128

set http_proxy=%proxy%
set HTTP_PROXY=%proxy%
set https_proxy=%proxy%
set HTTPS_PROXY=%proxy%

set REQUESTS_CA_BUNDLE=C:\ProgramData\orey\cert\cacert.pem
set CURL_CA_BUNDLE=C:\ProgramData\orey\cert\cacert.pem
set SSL_CERT_DIR=C:\ProgramData\orey\cert
set SSL_CERT_FILE=C:\ProgramData\orey\cert\cacert.pem
set PIP_CONFIG_FILE=C:\ProgramData\orey\home\pip.ini

echo Variables set
echo node version
node --version
echo npm version
npm --version
