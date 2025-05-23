{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2010-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}


var
  _kCFTypeArrayCallBacks: Pointer = nil;

function kCFTypeArrayCallBacks: CFArrayCallBacks;
begin
  if _kCFTypeArrayCallBacks = nil then
    _kCFTypeArrayCallBacks := dlsym(CFModule, MarshaledAString('kCFTypeArrayCallBacks'));
  Result := CFArrayCallBacks(_kCFTypeArrayCallBacks^);
end;


