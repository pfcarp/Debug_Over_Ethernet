# DoEth

## Dependency
```bash
sudo apt install libgraphviz-dev graphviz
sudo apt install doctest-dev
sudo apt install libpcap-dev
```

## Allow interface sniffing
```bash
sudo setcap cap_net_raw,cap_net_admin=eip ./doeth.out 
getcap ./doeth.out
```
