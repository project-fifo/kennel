#!/bin/bash

## Begin user configurable variables

SSL_ORG="Canine Cloud Compute"
SSL_UNIT="Cloud Ops"
SSL_EMAIL="admin@fifo.cloud"

AWK=/usr/bin/awk
SED=/usr/bin/sed
CURL=/opt/local/bin/curl
CERTDIR="/data/fifo/ca"
CERTPREFIX="fifo"
DAYS=1825 # 5 years

LOCAL_INFO=$($CURL -s ipinfo.io)


if ifconfig net1 > /dev/null 2>&1
then
  IP=`ifconfig net1 | grep inet | $AWK '{print $2}'`
else
  IP=`ifconfig net0 | grep inet | $AWK '{print $2}'`
fi

CERTSUBJECT="
C=$(echo $LOCAL_INFO | json country)
ST=$(echo $LOCAL_INFO | json region)
O=$(echo $SSL_ORG)
localityName=$(echo $LOCAL_INFO | json city)
commonName=$IP
organizationalUnitName=$(echo $SSL_UNIT)
emailAddress=$(echo SSL_EMAIL)"

SNARLCONF="/data/snarl/etc/snarl.conf"
KENNELCONF="/data/kennel/etc/kennel.conf"
HOWLCONF="/data/howl/etc/howl.conf"

USER=howl
GROUP=$USER

fail_if_error() {
  [ $1 != 0 ] && {
    echo "Error prior to line $2: $3"
    exit 10
  }
}

# before doing anything backup the current cert dir
mkdir -p $CERTDIR/backups
find $CERTDIR -maxdepth 1 -type f -exec basename {} \; | \
    xargs -n1 -I{} cp $CERTDIR/{} $CERTDIR/backups/{}.$(date +"%d%m%y-%H%M")


# Generate CA key pair
openssl genrsa -out $CERTDIR/$CERTPREFIX-ca.key 2048 >/dev/null 2>&1
fail_if_error $? $LINENO "Could not create key."

openssl req -new -x509 -nodes -batch -subj "$(echo "$CERTSUBJECT" | tr "\n" "/")" \
        -key $CERTDIR/$CERTPREFIX-ca.key -out $CERTDIR/$CERTPREFIX-ca.pem >/dev/null 2>&1
fail_if_error $? $LINENO "Could not create pem."

# Create client key pair for howl/kennel
openssl genrsa -out $CERTDIR/$CERTPREFIX.key 2048 >/dev/null 2>&1
fail_if_error $? $LINENO "Could not create client key pair"

openssl req -new -batch -subj "$(echo "$CERTSUBJECT" | tr "\n" "/")" \
        -key $CERTDIR/$CERTPREFIX.key -out $CERTDIR/$CERTPREFIX.csr -nodes >/dev/null 2>&1
fail_if_error $? $LINENO "Could not create certificate request."

openssl x509 -extfile <(printf "subjectAltName = IP:${IP}") \
        -req -in $CERTDIR/$CERTPREFIX.csr -CA $CERTDIR/$CERTPREFIX-ca.pem \
        -CAkey $CERTDIR/$CERTPREFIX-ca.key -CAcreateserial \
        -out $CERTDIR/$CERTPREFIX.crt -days $DAYS >/dev/null 2>&1
fail_if_error $? $LINENO "Could not create x509 request."

cat $CERTDIR/$CERTPREFIX.key $CERTDIR/$CERTPREFIX.crt > $CERTDIR/$CERTPREFIX.pem

# Config Snarl
$SED -i "" "s,^\(ssl\.ca_cert\s*=\s*\).*$,\1${CERTDIR}/${CERTPREFIX}-ca.pem," $SNARLCONF
$SED -i "" 's,^\(ssl\.ca_key\s*=\s*\).*$,\1'$CERTDIR'/'$CERTPREFIX'-ca.key,' $SNARLCONF

# Config Kennel
$SED -i "" 's,^\(ssl\.cacertfile\s*=\s*\).*$,\1'$CERTDIR'/'$CERTPREFIX'-ca.pem,' $KENNELCONF
$SED -i "" 's,^\(ssl\.certfile\s*=\s*\).*$,\1'$CERTDIR'/'$CERTPREFIX'-ca.key,' $KENNELCONF
$SED -i "" 's,^\(ssl\.keyfile\s*=\s*\).*$,\1'$CERTDIR'/'$CERTPREFIX'.key,' $KENNELCONF

# Config Howl
$SED -i "" 's,^\(ssl\.cacertfile\s*=\s*\).*$,\1'$CERTDIR'/'$CERTPREFIX'-ca.pem,' $HOWLCONF
$SED -i "" 's,^\(ssl\.certfile\s*=\s*\).*$,\1'$CERTDIR'/'$CERTPREFIX'-ca.key,' $HOWLCONF
$SED -i "" 's,^\(ssl\.keyfile\s*=\s*\).*$,\1'$CERTDIR'/'$CERTPREFIX'.key,' $HOWLCONF

cat <<EOF
*****************************************************************************
*                                                                           *
*  CA Setup complete.                                                       *
*                                                                           *
*  Howl, Kennel, and Snarl configurations have been updated.                *
*                                                                           *
*  Please restart with:                                                     *
*  svcadm restart howl                                                      *
*  svcadm restart kennel                                                    *
*  svcadm restart snarl                                                     *
*  svcs snarl kennel                                                        *
*                                                                           *
*****************************************************************************
EOF
