{*******************************************************}
{                                                       }
{             Delphi REST Client Framework              }
{                                                       }
{ Copyright(c) 2014-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{$HPPEMIT LINKUNIT}
unit REST.Backend.KinveyPushDevice;

interface

uses
  System.Classes,
  System.SysUtils,
  System.JSON,
  System.PushNotification,
  REST.Backend.Providers,
  REST.Backend.PushTypes,
  REST.Backend.KinveyProvider,
  REST.Backend.KinveyApi,
  REST.Backend.Exception;

type

{$IFDEF IOS}
{$DEFINE PUSH}
{$ENDIF}
{$IFDEF ANDROID}
{$DEFINE PUSH}
{$ENDIF}

  TKinveyPushDeviceAPI = class(TKinveyServiceAPIAuth, IBackendPushDeviceApi)
  private const
    sKinvey = 'Kinvey';
  protected
    { IBackendPushDeviceAPI }
    function GetPushService: TPushService; // May raise exception
    function HasPushService: Boolean;
    procedure RegisterDevice(AOnRegistered: TDeviceRegisteredAtProviderEvent);
    procedure UnregisterDevice;
  end;

  TKinveyPushDeviceService = class(TKinveyBackendService<TKinveyPushDeviceAPI>, IBackendService, IBackendPushDeviceService)
  protected
    { IBackendPushDeviceService }
    function CreatePushDeviceApi: IBackendPushDeviceApi;
    function GetPushDeviceApi: IBackendPushDeviceApi;
  end;

  EKinveyPushNotificationError = class(EBackendServiceError); 

implementation

uses
  System.Generics.Collections,
  System.TypInfo,
  REST.Backend.Consts,
  REST.Backend.ServiceFactory
{$IFDEF PUSH}
{$IFDEF IOS}
  ,FMX.PushNotification.IOS // Inject iOS push provider
{$ENDIF}
{$IFDEF ANDROID}
  ,FMX.PushNotification.Android // Inject Android push provider
{$ENDIF}
{$ENDIF}
  ;

{ TKinveyPushDeviceService }

function TKinveyPushDeviceService.CreatePushDeviceApi: IBackendPushDeviceApi;
begin
  Result := CreateBackendApi;
end;

function TKinveyPushDeviceService.GetPushDeviceApi: IBackendPushDeviceApi;
begin
  EnsureBackendApi;
  Result := BackendAPI;
end;

{ TKinveyPushDeviceAPI }

function GetDeviceID(const AService: TPushService): string;
begin
  Result := AService.DeviceIDValue[TPushService.TDeviceIDNames.DeviceID];
end;

function GetDeviceName: string;
begin
{$IFDEF IOS}
  Result := 'ios';   // Do not localize
{$ENDIF}
{$IFDEF ANDROID}
  Result := 'android';  // Do not localize
{$ENDIF}
{$IFDEF MSWINDOWS}
  Result := 'windows';  // Do not localize
{$ENDIF}
end;

function GetServiceName: string;
begin
{$IFDEF PUSH}
{$IFDEF IOS}
  Result := TPushService.TServiceNames.APS;
{$ENDIF}
{$IFDEF ANDROID}
  Result := TPushService.TServiceNames.FCM;
{$ENDIF}
{$IFDEF MSWINDOWS}
  Result := '';
{$ENDIF}
{$ENDIF}
end;


function GetService(const AServiceName: string): TPushService;
begin
  Result := TPushServiceManager.Instance.GetServiceByName(AServiceName);
end;

procedure GetRegistrationInfo(const APushService: TPushService;
  out ADeviceID, ADeviceToken: string);
begin
  ADeviceID := APushService.DeviceIDValue[TPushService.TDeviceIDNames.DeviceID];
  ADeviceToken := APushService.DeviceTokenValue[TPushService.TDeviceTokenNames.DeviceToken];

  if ADeviceID = ''  then
    raise EKinveyPushNotificationError.Create(sDeviceIDUnavailable); 

  if ADeviceToken = ''  then
    raise EKinveyPushNotificationError.Create(sDeviceTokenUnavailable);   
end;

function TKinveyPushDeviceAPI.GetPushService: TPushService;
var
  LServiceName: string;
  LDeviceName: string;
begin
  LDeviceName := GetDeviceName;
  Assert(LDeviceName <> '');
  LServiceName := GetServiceName;
  if LServiceName = '' then
    raise EKinveyPushNotificationError.CreateFmt(sPushDeviceNoPushService, [sKinvey, LDeviceName]);

  Result := GetService(LServiceName);
  if Result = nil then
    raise EKinveyPushNotificationError.CreateFmt(sPushDevicePushServiceNotFound, [sKinvey, LServiceName]);
end;

function TKinveyPushDeviceAPI.HasPushService: Boolean;
var
  LServiceName: string;
begin
  LServiceName := GetServiceName;
  Result := (LServiceName <> '') and (GetService(LServiceName) <> nil);
end;


procedure TKinveyPushDeviceAPI.RegisterDevice(
  AOnRegistered: TDeviceRegisteredAtProviderEvent);
var
  LDeviceName: string;
  LServiceName: string;
{$IFDEF PUSH}
  LDeviceID: string;
  LDeviceToken: string;
{$ENDIF}
begin
  LDeviceName := GetDeviceName;
  Assert(LDeviceName <> '');
  LServiceName := GetServiceName;
  if LServiceName = '' then
    raise EKinveyPushNotificationError.CreateFmt(sPushDeviceNoPushService, [sKinvey, LDeviceName]);

{$IFDEF PUSH}
  GetRegistrationInfo(GetPushService, LDeviceID, LDeviceToken);    // May raise exception
{$IFDEF IOS}
  KinveyAPI.PushRegisterDevice(TKinveyApi.TPlatformType.IOS, LDeviceToken);
{$ELSE}
  KinveyAPI.PushRegisterDevice(TKinveyApi.TPlatformType.Android, LDeviceToken);
{$ENDIF}
  if Assigned(AOnRegistered) then
    AOnRegistered(GetPushService);
{$ELSE}
  raise EKinveyPushNotificationError.CreateFmt(sPushDeviceNoPushService, [sKinvey, LDeviceName]);
{$ENDIF}
end;

procedure TKinveyPushDeviceAPI.UnregisterDevice;
{$IFDEF PUSH}
var
  LDeviceID: string;
  LDeviceToken: string;
{$ENDIF}
begin
{$IFDEF PUSH}
  GetRegistrationInfo(GetPushService, LDeviceID, LDeviceToken);    // May raise exception
{$IFDEF IOS}
  KinveyAPI.PushUnregisterDevice(TKinveyApi.TPlatformType.IOS, LDeviceToken);
{$ELSE}
  KinveyAPI.PushUnregisterDevice(TKinveyApi.TPlatformType.Android, LDeviceToken);
{$ENDIF}
{$ENDIF}
end;


type
  TKinveyPushDeviceServiceFactory = class(TProviderServiceFactory<IBackendPushDeviceService>)
  protected
    function CreateService(const AProvider: IBackendProvider; const IID: TGUID): IBackendService; override;
  public
    constructor Create;
  end;

constructor TKinveyPushDeviceServiceFactory.Create;
begin
  inherited Create(TCustomKinveyProvider.ProviderID, 'REST.Backend.KinveyPushDevice');   // Do not localize
end;

function TKinveyPushDeviceServiceFactory.CreateService(const AProvider: IBackendProvider;
  const IID: TGUID): IBackendService;
begin
  Result := TKinveyPushDeviceService.Create(AProvider);
end;

var
  FFactory: TKinveyPushDeviceServiceFactory;

initialization
  FFactory := TKinveyPushDeviceServiceFactory.Create;
  FFactory.Register;

finalization
  FFactory.Unregister;
  FFactory.Free;

end.
