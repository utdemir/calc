let
hs = import ./calc-hs;
ts = import ./calc-ts;
in
{
 inherit (hs) calc-cli;
 inherit (ts) calc-web;
}
