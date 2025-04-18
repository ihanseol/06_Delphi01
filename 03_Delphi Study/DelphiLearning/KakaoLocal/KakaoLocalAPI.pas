﻿unit KakaoLocalAPI;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Variants,
  System.Net.HttpClient, System.Net.URLClient, Vcl.Clipbrd, System.Generics.Collections,
  System.NetEncoding, Vcl.Graphics, Winapi.Windows, Winapi.Messages,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;


type
  TKakaoLocalAPI = class
  private
    FREST_API_KEY: string;
    FHttpClient: THTTPClient;

    function ExtractCoordinates(const JsonData: TJSONArray): TPair<string, string>;
    procedure TransformCoordinates(const X, Y: string);
    function TransformCoordinatesReturn(const X, Y: string): string;
  public
    constructor Create(const API_KEY: string);
    destructor Destroy; override;

    procedure SearchAddress(const Address: string);
    function SearchAddressRerurn(const Address: string): TStringList;
  end;

implementation

{ TKakaoLocalAPI }

constructor TKakaoLocalAPI.Create(const API_KEY: string);
begin
  inherited Create;
  FREST_API_KEY := API_KEY;
  FHttpClient := THTTPClient.Create;
  FHttpClient.CustomHeaders['Authorization'] := Format('KakaoAK %s', [FREST_API_KEY]);
end;

destructor TKakaoLocalAPI.Destroy;
begin
  FHttpClient.Free;
  inherited;
end;

function TKakaoLocalAPI.ExtractCoordinates(const JsonData: TJSONArray): TPair<string, string>;
var
  FirstItem: TJSONObject;
begin
  Result := TPair<string, string>.Create('', '');

  if (JsonData = nil) or (JsonData.Count = 0) then
    Exit;

  FirstItem := JsonData.Items[0] as TJSONObject;
  Result.Key := FirstItem.GetValue<string>('x');
  Result.Value := FirstItem.GetValue<string>('y');
end;

procedure TKakaoLocalAPI.TransformCoordinates(const X, Y: string);
var
  URL: string;
  Response: IHTTPResponse;
  JsonData: TJSONObject;
  Documents: TJSONArray;
  TransformedX, TransformedY: string;
  CoordinateText: string;
  JsonObj: TJSONObject;
begin
  URL := Format('https://dapi.kakao.com/v2/local/geo/transcoord.json?x=%s&y=%s&input_coord=WGS84&output_coord=TM', [X, Y]);
  Response := FHttpClient.Get(URL);

  if Response.StatusCode = 200 then
  begin
    JsonData := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
    try
      Documents := JsonData.GetValue<TJSONArray>('documents');
      if (Documents <> nil) and (Documents.Count > 0) then
      begin
        JsonObj := Documents.Items[0] as TJSONObject;
        TransformedX := JsonObj.GetValue<string>('x');
        TransformedY := JsonObj.GetValue<string>('y');

        WriteLn(Format('x: %s', [TransformedX]));
        WriteLn(Format('y: %s', [TransformedY]));
        WriteLn('----------------------------------------------------------------------------------------------------');

        CoordinateText := Format('%s,%s', [TransformedX, TransformedY]);
        Clipboard.AsText := CoordinateText;
        WriteLn(CoordinateText);
      end;
    finally
      JsonData.Free;
    end;
  end;
end;


procedure TKakaoLocalAPI.SearchAddress(const Address: string);
var
  URL: string;
  Response: IHTTPResponse;
  JsonData: TJSONObject;
  Documents: TJSONArray;
  AddressData: string;
  Coordinates: TPair<string, string>;
  JsonObj: TJSONObject;
  AddressObj: TJSONObject;
begin
  URL := 'https://dapi.kakao.com/v2/local/search/address.json';

  WriteLn('----------------------------------------------------------------------------------------------------');

  Response := FHttpClient.Get(URL + '?query=' + TNetEncoding.URL.Encode(Address));

  if Response.StatusCode = 200 then
  begin
    JsonData := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
    try
      Documents := JsonData.GetValue<TJSONArray>('documents');
      WriteLn(Format('documents: %s', [Documents.ToJSON]));

      AddressData := '';

      if (Documents <> nil) and (Documents.Count > 0) then
      begin
        JsonObj := Documents.Items[0] as TJSONObject;
        AddressObj := JsonObj.GetValue<TJSONObject>('address');
        AddressData := AddressObj.GetValue<string>('address_name');
      end;

      WriteLn('----------------------------------------------------------------------------------------------------');
      WriteLn(AddressData);
      WriteLn('----------------------------------------------------------------------------------------------------');

      Coordinates := ExtractCoordinates(Documents);
      WriteLn(Format('x: %s, y: %s', [Coordinates.Key, Coordinates.Value]));
      WriteLn('----------------------------------------------------------------------------------------------------');

      if (Coordinates.Key <> '') and (Coordinates.Value <> '') then
        TransformCoordinates(Coordinates.Key, Coordinates.Value);

      WriteLn('----------------------------------------------------------------------------------------------------');
      Write('종료하시려면 엔터키를 누르세요 ~ ');
      ReadLn;
    finally
      JsonData.Free;
    end;
  end;
end;



function TKakaoLocalAPI.TransformCoordinatesReturn(const X, Y: string): string;
var
  URL: string;
  Response: IHTTPResponse;
  JsonData: TJSONObject;
  Documents: TJSONArray;
  TransformedX, TransformedY: string;
  CoordinateText: string;
  JsonObj: TJSONObject;
begin
  URL := Format('https://dapi.kakao.com/v2/local/geo/transcoord.json?x=%s&y=%s&input_coord=WGS84&output_coord=TM', [X, Y]);
  Response := FHttpClient.Get(URL);

  if Response.StatusCode = 200 then
  begin
    JsonData := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
    try
      Documents := JsonData.GetValue<TJSONArray>('documents');
      if (Documents <> nil) and (Documents.Count > 0) then
      begin
        JsonObj := Documents.Items[0] as TJSONObject;
        TransformedX := JsonObj.GetValue<string>('x');
        TransformedY := JsonObj.GetValue<string>('y');

        CoordinateText := Format('%s,  %s', [TransformedX, TransformedY]);
        Clipboard.AsText := CoordinateText;
        Result := CoordinateText;
      end;
    finally
      JsonData.Free;
    end;
  end;
end;



function TKakaoLocalAPI.SearchAddressRerurn(const Address: string): TStringList;
var
  URL: string;
  Response: IHTTPResponse;
  JsonData: TJSONObject;
  Documents: TJSONArray;
  AddressData, CoordinateText : string;
  Coordinates: TPair<string, string>;
  JsonObj: TJSONObject;
  AddressObj: TJSONObject;
   MyList: TStringList;

begin
  URL := 'https://dapi.kakao.com/v2/local/search/address.json';
  Response := FHttpClient.Get(URL + '?query=' + TNetEncoding.URL.Encode(Address));
  MyList := TStringList.Create;

  if Response.StatusCode = 200 then
  begin
    JsonData := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
    try
      Documents := JsonData.GetValue<TJSONArray>('documents');
//      Showmessage(Format('documents: %s', [Documents.ToJSON]));

      AddressData := '';

      if (Documents <> nil) and (Documents.Count > 0) then
      begin
        JsonObj := Documents.Items[0] as TJSONObject;
        AddressObj := JsonObj.GetValue<TJSONObject>('address');
        AddressData := AddressObj.GetValue<string>('address_name');
      end;

      Coordinates := ExtractCoordinates(Documents);
      Mylist.add(AddressData);
      Mylist.add( Format('x: %s,  y: %s', [Coordinates.Key, Coordinates.Value]));

      if (Coordinates.Key <> '') and (Coordinates.Value <> '') then
        AddressData := TransformCoordinatesReturn(Coordinates.Key, Coordinates.Value);
        Mylist.add(AddressData);
        Result := Mylist

    finally
      JsonData.Free;
    end;
  end;
end;

end.
