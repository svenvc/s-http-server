readme.txt file describing how the SSL files where generated.

Basically, steps 1 and 2 as described in

	http://www.vanemery.com/Linux/Apache/apache-SSL.html

were followed to finally generate a server key and a self-signed server certificate.

There are many other ways to arrive at the same results, and many more options.


*** dhparam.pem ***

this file contains the Diffie-Hellman parameters.

it was generated as follows:

openssl dhparam -out /Users/sven/darcs/s-http-server/rsrc/dhparam.pem 1024

its contents can be viewed as follows:

openssl dhparam -in /Users/sven/darcs/s-http-server/rsrc/dhparam.pem -text
Diffie-Hellman-Parameters: (1024 bit)
    prime:
        00:d7:16:bf:07:37:ff:46:f1:98:b3:11:61:b4:b5:
        91:35:14:7b:a7:af:ec:c3:87:f4:cb:ba:34:ea:c3:
        f2:3f:01:43:0d:e9:b2:4f:e3:0e:1d:87:4a:de:33:
        c6:65:07:71:a5:ba:6a:18:c9:1b:4b:91:2c:f9:2f:
        aa:c5:12:f2:4c:63:bc:82:8f:15:c1:7e:40:d5:0e:
        93:5b:29:45:35:88:b3:61:27:1b:51:2d:07:91:52:
        dd:ba:ff:a1:9e:0d:68:c1:b5:e7:8c:23:c4:52:d0:
        2c:0a:ba:f1:80:57:d1:6f:62:92:79:41:b7:4b:d9:
        a3:f7:73:8e:d0:7a:26:a0:f3
    generator: 2 (0x2)
-----BEGIN DH PARAMETERS-----
MIGHAoGBANcWvwc3/0bxmLMRYbS1kTUUe6ev7MOH9Mu6NOrD8j8BQw3psk/jDh2H
St4zxmUHcaW6ahjJG0uRLPkvqsUS8kxjvIKPFcF+QNUOk1spRTWIs2EnG1EtB5FS
3br/oZ4NaMG154wjxFLQLAq68YBX0W9iknlBt0vZo/dzjtB6JqDzAgEC
-----END DH PARAMETERS-----


*** test-server.key ***

this file contains the RSA private key.

it was generated as follows:

openssl genrsa -des3 -out test-server.key 1024

its contents can be viewed as follows (publishing this info reveals the key ;-):

openssl rsa -in test-server.key -text
Enter pass phrase for test-server.key:
Private-Key: (1024 bit)
modulus:
    00:e6:1d:b6:28:ec:b9:b9:60:83:fb:6b:08:11:be:
    37:60:d3:3b:63:df:3d:6d:52:57:df:5b:40:25:d0:
    11:a3:ab:9f:d1:44:99:53:66:e7:d3:48:a9:16:f2:
    f2:60:f4:53:06:57:12:50:33:93:00:d1:0e:30:cb:
    a1:4c:a8:20:27:7f:a2:1c:ec:27:d2:f5:01:d2:b7:
    e2:53:a5:44:68:7a:bb:58:16:34:c2:e3:b7:dc:34:
    b7:9f:11:8d:05:bc:c4:a2:68:0d:d3:48:52:0b:1a:
    ba:db:c4:c5:3a:70:48:a4:d7:cb:bf:2b:af:6e:82:
    c9:5e:97:68:a2:72:4c:36:cb
publicExponent: 65537 (0x10001)
privateExponent:
    57:a1:46:e6:2c:c7:c9:25:4c:fd:68:53:e9:55:d3:
    86:e6:c9:be:0d:9c:39:ce:5a:b2:2e:f0:ad:b5:9f:
    92:01:60:59:f3:d2:a3:a5:13:71:2d:41:5f:00:e3:
    76:32:74:8f:7c:86:f7:cd:bc:14:5a:88:19:e2:e1:
    a8:ec:79:59:78:60:4b:66:14:ef:5e:20:0d:a7:0f:
    e7:2c:f6:5b:79:89:db:80:fc:4d:e5:92:28:24:7f:
    b2:8d:4f:2d:83:fd:19:07:cd:38:53:67:40:16:87:
    f6:86:6c:59:1d:59:21:37:5d:1f:51:4f:d5:35:9b:
    6e:48:a5:92:38:90:c9:11
prime1:
    00:f9:39:ce:85:00:ca:63:a0:62:29:ae:99:f9:06:
    ac:98:42:2f:8b:c6:4d:6f:c4:a8:0e:f4:71:95:6d:
    36:f4:38:b7:7d:34:66:4e:d1:b7:61:9c:0b:ca:3a:
    eb:28:49:51:7a:42:ff:87:d4:41:34:d5:50:54:24:
    b1:6a:5e:a3:e9
prime2:
    00:ec:5e:ee:de:a1:4c:ca:75:c8:69:37:b1:4b:30:
    28:98:a2:a6:6c:95:6d:c5:6f:a9:a1:bf:89:23:07:
    75:38:e1:c8:c0:0d:46:d9:cf:3b:ea:d9:50:f9:8e:
    8d:03:cd:52:f8:73:ae:c3:04:70:28:c8:88:9d:78:
    fc:33:3c:58:93
exponent1:
    0f:49:90:75:70:1a:fa:09:78:7b:fe:0d:cb:cc:b1:
    01:95:ed:bc:b1:29:46:d5:d5:49:35:8d:52:11:24:
    f1:ce:18:d3:41:47:95:46:1f:ed:88:d8:e0:4a:c4:
    e9:ef:b5:63:be:80:56:20:9a:ef:56:b6:5a:b2:f5:
    7f:04:d7:21
exponent2:
    00:d4:e3:43:dc:fc:05:ff:ab:49:8f:8a:7b:82:2e:
    a3:c1:a5:6c:a3:0b:8a:cc:72:1f:a3:f0:b0:80:fe:
    2c:93:c8:b7:58:52:1c:e7:fb:80:09:ab:25:05:3f:
    60:be:75:e5:2e:a4:72:58:6e:dc:dd:be:8f:5c:d5:
    24:c0:b8:af:45
coefficient:
    00:d4:11:23:a9:ba:18:76:67:a1:6e:3b:74:d7:e8:
    19:40:45:de:f3:87:65:74:df:1c:12:c8:ca:24:6f:
    69:8d:9a:0e:07:14:95:ec:af:de:65:4f:04:1c:8f:
    2e:94:48:88:8a:88:7b:6c:69:ce:5a:ba:5a:11:f6:
    9c:de:34:cb:28
writing RSA key
-----BEGIN RSA PRIVATE KEY-----
MIICXQIBAAKBgQDmHbYo7Lm5YIP7awgRvjdg0ztj3z1tUlffW0Al0BGjq5/RRJlT
ZufTSKkW8vJg9FMGVxJQM5MA0Q4wy6FMqCAnf6Ic7CfS9QHSt+JTpURoertYFjTC
47fcNLefEY0FvMSiaA3TSFILGrrbxMU6cEik18u/K69ugslel2iickw2ywIDAQAB
AoGAV6FG5izHySVM/WhT6VXThubJvg2cOc5asi7wrbWfkgFgWfPSo6UTcS1BXwDj
djJ0j3yG9828FFqIGeLhqOx5WXhgS2YU714gDacP5yz2W3mJ24D8TeWSKCR/so1P
LYP9GQfNOFNnQBaH9oZsWR1ZITddH1FP1TWbbkilkjiQyRECQQD5Oc6FAMpjoGIp
rpn5BqyYQi+Lxk1vxKgO9HGVbTb0OLd9NGZO0bdhnAvKOusoSVF6Qv+H1EE01VBU
JLFqXqPpAkEA7F7u3qFMynXIaTexSzAomKKmbJVtxW+pob+JIwd1OOHIwA1G2c87
6tlQ+Y6NA81S+HOuwwRwKMiInXj8MzxYkwJAD0mQdXAa+gl4e/4Ny8yxAZXtvLEp
RtXVSTWNUhEk8c4Y00FHlUYf7YjY4ErE6e+1Y76AViCa71a2WrL1fwTXIQJBANTj
Q9z8Bf+rSY+Ke4Iuo8GlbKMLisxyH6PwsID+LJPIt1hSHOf7gAmrJQU/YL515S6k
clhu3N2+j1zVJMC4r0UCQQDUESOpuhh2Z6FuO3TX6BlARd7zh2V03xwSyMokb2mN
mg4HFJXsr95lTwQcjy6USIiKiHtsac5auloR9pzeNMso
-----END RSA PRIVATE KEY-----


*** test-server.crt ***

this file contains the self-signed certificate.

it was generated as follows:

(first setup a - dummy - certification authority)
 
openssl genrsa -des3 -out my-ca.key 2048
openssl req -new -x509 -days 3650 -key my-ca.key -out my-ca.crt

[openssl x509 -in my-ca.crt -text -noout]

(next generate a request and have it signed by the certification authority)

openssl req -new -key test-server.key -out test-server.csr
openssl x509 -req -in test-server.csr -out test-server.crt -sha1 -CA my-ca.crt -CAkey my-ca.key -CAcreateserial -days 3650

its contents can be viewed as follows:

openssl x509 -in test-server.crt -text
Certificate:
    Data:
        Version: 1 (0x0)
        Serial Number:
            c6:0e:6a:7a:39:e1:45:9f
        Signature Algorithm: sha1WithRSAEncryption
        Issuer: C=BE, ST=Flanders, L=Hasselt, O=Beta Nine BVBA, OU=software engineering, CN=Sven Van Caekenberghe/emailAddress=sven@beta9.be
        Validity
            Not Before: Apr  7 14:17:34 2006 GMT
            Not After : Apr  4 14:17:34 2016 GMT
        Subject: C=BE, ST=Flanders, L=Hasselt, O=Beta Nine BVBA, OU=software engineering, CN=localhost/emailAddress=sven@beta9.be
        Subject Public Key Info:
            Public Key Algorithm: rsaEncryption
            RSA Public Key: (1024 bit)
                Modulus (1024 bit):
                    00:e6:1d:b6:28:ec:b9:b9:60:83:fb:6b:08:11:be:
                    37:60:d3:3b:63:df:3d:6d:52:57:df:5b:40:25:d0:
                    11:a3:ab:9f:d1:44:99:53:66:e7:d3:48:a9:16:f2:
                    f2:60:f4:53:06:57:12:50:33:93:00:d1:0e:30:cb:
                    a1:4c:a8:20:27:7f:a2:1c:ec:27:d2:f5:01:d2:b7:
                    e2:53:a5:44:68:7a:bb:58:16:34:c2:e3:b7:dc:34:
                    b7:9f:11:8d:05:bc:c4:a2:68:0d:d3:48:52:0b:1a:
                    ba:db:c4:c5:3a:70:48:a4:d7:cb:bf:2b:af:6e:82:
                    c9:5e:97:68:a2:72:4c:36:cb
                Exponent: 65537 (0x10001)
    Signature Algorithm: sha1WithRSAEncryption
        76:2e:dc:b5:bd:42:8d:4f:ea:6a:83:ad:89:57:ae:a1:a2:a5:
        66:49:4a:41:c6:14:fd:e6:1f:e9:d6:c8:89:1c:4c:e1:e3:41:
        7d:74:2c:3f:95:59:f5:9c:97:70:a7:e9:be:87:25:b0:b6:48:
        14:59:48:3f:87:28:42:f2:fe:4b:cf:5d:08:20:7c:86:1b:ef:
        d0:b2:70:16:13:28:b6:f0:c8:3c:56:f7:4f:01:be:c6:6b:c1:
        b3:c1:c1:26:6e:d8:a5:1f:12:8d:2a:fb:18:cb:38:4a:7f:30:
        98:69:ac:81:fb:e7:99:e2:4d:ed:f6:b6:0e:97:01:19:55:5c:
        3d:b3:fe:fc:5c:de:cd:01:dd:fd:e6:36:67:25:f5:81:82:7d:
        ca:35:6b:92:8b:68:dc:0c:9f:b3:ef:88:c9:e4:9b:bd:02:0f:
        53:e4:43:58:12:7e:05:05:22:2b:c8:47:e1:bd:dd:7e:df:ce:
        51:d5:b3:df:64:ab:a2:8f:8b:f0:2a:58:b4:9d:7f:5d:ca:33:
        bc:64:02:a7:87:da:86:56:8a:cd:91:5d:a2:c9:17:f1:38:69:
        79:d0:b8:23:fb:06:70:1b:0f:28:73:22:4e:6f:c7:c7:20:69:
        55:98:07:3b:b0:91:a2:df:34:5c:78:8c:6f:d9:e6:68:de:6a:
        cd:56:9c:87
-----BEGIN CERTIFICATE-----
MIIDPjCCAiYCCQDGDmp6OeFFnzANBgkqhkiG9w0BAQUFADCBqDELMAkGA1UEBhMC
QkUxETAPBgNVBAgTCEZsYW5kZXJzMRAwDgYDVQQHEwdIYXNzZWx0MRcwFQYDVQQK
Ew5CZXRhIE5pbmUgQlZCQTEdMBsGA1UECxMUc29mdHdhcmUgZW5naW5lZXJpbmcx
HjAcBgNVBAMTFVN2ZW4gVmFuIENhZWtlbmJlcmdoZTEcMBoGCSqGSIb3DQEJARYN
c3ZlbkBiZXRhOS5iZTAeFw0wNjA0MDcxNDE3MzRaFw0xNjA0MDQxNDE3MzRaMIGc
MQswCQYDVQQGEwJCRTERMA8GA1UECBMIRmxhbmRlcnMxEDAOBgNVBAcTB0hhc3Nl
bHQxFzAVBgNVBAoTDkJldGEgTmluZSBCVkJBMR0wGwYDVQQLExRzb2Z0d2FyZSBl
bmdpbmVlcmluZzESMBAGA1UEAxMJbG9jYWxob3N0MRwwGgYJKoZIhvcNAQkBFg1z
dmVuQGJldGE5LmJlMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDmHbYo7Lm5
YIP7awgRvjdg0ztj3z1tUlffW0Al0BGjq5/RRJlTZufTSKkW8vJg9FMGVxJQM5MA
0Q4wy6FMqCAnf6Ic7CfS9QHSt+JTpURoertYFjTC47fcNLefEY0FvMSiaA3TSFIL
GrrbxMU6cEik18u/K69ugslel2iickw2ywIDAQABMA0GCSqGSIb3DQEBBQUAA4IB
AQB2Lty1vUKNT+pqg62JV66hoqVmSUpBxhT95h/p1siJHEzh40F9dCw/lVn1nJdw
p+m+hyWwtkgUWUg/hyhC8v5Lz10IIHyGG+/QsnAWEyi28Mg8VvdPAb7Ga8GzwcEm
btilHxKNKvsYyzhKfzCYaayB++eZ4k3t9rYOlwEZVVw9s/78XN7NAd395jZnJfWB
gn3KNWuSi2jcDJ+z74jJ5Ju9Ag9T5ENYEn4FBSIryEfhvd1+385R1bPfZKuij4vw
Kli0nX9dyjO8ZAKnh9qGVorNkV2iyRfxOGl50Lgj+wZwGw8ocyJOb8fHIGlVmAc7
sJGi3zRceIxv2eZo3mrNVpyH
-----END CERTIFICATE-----


Note that the Lisp code uses the standard OpenSSL libraries so please refer to 
the appropriate OpenSSL documentation for more information.



