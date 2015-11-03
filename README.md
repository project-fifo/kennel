kennel
=====

An OTP application

Build
-----

    $ rebar3 compile


Configuration
-------------

Set public and private network for the user
```
fifo users metadata e01d2b2c-178c-4b9d-9046-cba2471b2c27 set fifo.docker.networks.public fd060fd4-b5cd-4fef-911b-a5023d8b3bb4
fifo users metadata e01d2b2c-178c-4b9d-9046-cba2471b2c27 set fifo.docker.networks.private faa25949-e8a2-446f-b01a-ddf17f058016
```