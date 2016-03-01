#!/usr/bin/bash

USER=kennel
GROUP=$USER
AWK=/usr/bin/awk
SED=/usr/bin/sed

case $2 in
    PRE-INSTALL)
        if grep "^$GROUP:" /etc/group > /dev/null 2>&1
        then
            echo "Group already exists, skipping creation."
        else
            echo Creating kennel group ...
            groupadd $GROUP
        fi
        if id $USER > /dev/null 2>&1
        then
            echo "User already exists, skipping creation."
        else
            echo Creating kennel user ...
            useradd -g $GROUP -d /var/db/kennel -s /bin/false $USER
            echo "Granting permissions to use low port numbers"
            /usr/sbin/usermod -K defaultpriv=basic,net_privaddr $USER
        fi
        echo Creating directories ...
        mkdir -p /data/kennel/db/ring
        mkdir -p /data/kennel/log/sasl
        chown -R kennel:kennel /data/kennel

        if [ -d /tmp/sniffle ]
        then
            chown -R $USER:$GROUP /tmp/sniffle/
        fi

        ;;
    POST-INSTALL)
        svccfg import /opt/local/fifo-kennel/share/kennel.xml
        echo Trying to guess configuration ...
        IP=`ifconfig net0 | grep inet | $AWK '{print $2}'`

        CONFFILE=/data/kennel/etc/kennel.conf
        cp /opt/local/fifo-kennel/etc/kennel.conf.example ${CONFFILE}.example

        if [ ! -f "${CONFFILE}" ]
        then
            echo "Creating new configuration from example file."
            cp ${CONFFILE}.example ${CONFFILE}
            $SED -i bak -e "s/127.0.0.1/${IP}/g" ${CONFFILE}
        else
            echo "Please make sure you update your config according to the update manual!"
            #/opt/local/fifo-sniffle/share/update_config.sh ${CONFFILE}.example ${CONFFILE} > ${CONFFILE}.new &&
            #    mv ${CONFFILE} ${CONFFILE}.old &&
            #    mv ${CONFFILE}.new ${CONFFILE}
        fi
        ;;
esac
