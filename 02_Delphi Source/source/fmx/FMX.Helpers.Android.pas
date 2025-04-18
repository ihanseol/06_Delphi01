{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{                                                       }
{           Helpers for Android implementations         }
{                                                       }
{ Copyright(c) 2013-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Helpers.Android;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.App,
  Androidapi.Helpers, Androidapi.JNIBridge, FMX.Surfaces, FMX.Graphics, FMX.Controls;

const
  /// <summary>Minimum base action number for using in Android Intent</summary>
  UserRequestCodeBase = 10;
  
function GetNativeTheme: Integer; overload;
function GetNativeTheme(const AControl: TControl): Integer; overload;

{ Bitmap conversions }

function JBitmapToBitmap(const SrcBitmap: JBitmap; const DstBitmap: TBitmap): Boolean;
function BitmapToJBitmap(const SrcBitmap: TBitmap; const DstBitmap: JBitmap): Boolean;

{ Surface conversions }

function JBitmapToSurface(const SrcBitmap: JBitmap; const DestSurface: TBitmapSurface): Boolean;
function SurfaceToJBitmap(const SrcSurface: TBitmapSurface; const DestBitmap: JBitmap): Boolean;
function NativeViewToSurface(const ANativeView: JView; const ASurface: TBitmapSurface): Boolean;

{ Java array conversion }

function CreateJavaStringArray(const ASource: array of const): TJavaObjectArray<JString>;

{ Work with UI thread }

type
  TMethodCallback = procedure of object;
  TCallBack = reference to procedure;

type
  TRunnable = class (TJavaLocal, JRunnable)
  strict private
    FActivity: JActivity;
    FMethodCallback: TMethodCallback;
    FCallback: TCallBack;
    FFinished: Boolean;
  public
    constructor Create; overload;
    constructor Create(const ACallback: TCallBack); overload;
    constructor Create(const AMethodCallback: TMethodCallback); overload;
    procedure Start;
    { JRunnable }
    procedure run; cdecl;
    property Finished: Boolean read FFinished;
  end;

  TUIThreadCaller = class
  public type
    TMethodProc = procedure of object;
    TMethodFunc<R> = function: R of object;
    TMethodFunc<A; R> = function(Arg: A): R of object;
    TMethodFunc<A; B; R> = function(Arg: A; Arg2: B): R of object;
    TMethodProc<A> = procedure(Arg: A) of object;
    TMethodProc2<A;B> = procedure(Arg1: A; Arg2: B) of object;
    TMethodProc3<A;B;C> = procedure(Arg1: A; Arg2: B; Arg3: C) of object;
  public
    class function Call<R>(const Func: TFunc<R>): R; overload;
    class function Call<R>(const Func: TMethodFunc<R>): R; overload;
    class function Call<A; R>(const Func: TFunc<A, R>; const Arg: A): R; overload;
    class function Call<A; R>(const Func: TMethodFunc<A, R>; const Arg: A): R; overload;
    class function Call<A; B; R>(const Func: TFunc<A, B, R>; const Arg1: A; const Arg2: B): R; overload;
    class function Call<A; B; R>(const Func: TMethodFunc<A, B, R>; const Arg1: A; const Arg2: B): R; overload;
    class procedure Call<A>(const Proc: TProc<A>; Arg: A); overload;
    class procedure Call<A>(const Proc: TMethodProc<A>; Arg: A); overload;
    class procedure Call<A;B>(const Proc: TMethodProc2<A,B>; Arg1: A; Arg2: B); overload;
    class procedure Call<A;B>(const Proc: TProc<A,B>; Arg1: A; Arg2: B); overload;
    class procedure Call<A;B;C>(const Proc: TMethodProc3<A,B,C>; Arg1: A; Arg2: B; Arg3: C); overload;
    class procedure InvokeIfNotNil<A:JObject>(const Proc: TProc<A>; const Arg: A);
    class function CallIfNotNil<A:JObject; R>(const Func: TFunc<A, R>; const Arg: A): R;
    /// <summary>Post UIThreadProc in Android UI thread.
    /// Post SyncProc to main thread queue after UIThreadProc returns.</summary>
    class procedure CallWithAsyncCallback(const UIThreadProc, SyncProc: TCallback);
    class procedure ForceRunnablesCollection;
  end;

procedure CallInUIThread(const AMethod: TMethodCallback); overload;
procedure CallInUIThread(const AMethod: TCallBack); overload;
procedure CallInUIThreadAndWaitFinishing(const AMethod: TMethodCallback); overload;
procedure CallInUIThreadAndWaitFinishing(const AMethod: TCallBack); overload;

implementation

uses
  Androidapi.Bitmap, FMX.Platform, FMX.Types, FMX.Styles, FMX.Forms, System.Generics.Collections,
  System.SyncObjs, System.Rtti, System.Classes, FMX.Consts;

const
  ANDROID_LIGHT_THEME = '[LIGHTSTYLE]';
  ANDROID_DARK_THEME = '[DARKSTYLE]';

function GetThemeFromDescriptor(ADescriptor: TStyleDescription): Integer;
begin
  Result := 0;
  if ADescriptor <> nil then
  begin
    if ADescriptor.PlatformTarget.Contains(ANDROID_LIGHT_THEME) then
      Result := TJAlertDialog.JavaClass.THEME_DEVICE_DEFAULT_LIGHT;
    if ADescriptor.PlatformTarget.Contains(ANDROID_DARK_THEME) then
      Result := TJAlertDialog.JavaClass.THEME_DEVICE_DEFAULT_DARK;
  end;
end;

function GetNativeTheme: Integer;
var
  LStyleDescriptor: TStyleDescription;
begin
  Result := 0;
  if (Screen <> nil) and (Screen.ActiveForm <> nil) then
  begin
    if Screen.ActiveForm.StyleBook <> nil then
      LStyleDescriptor := TStyleManager.FindStyleDescriptor(Screen.ActiveForm.StyleBook.Style)
    else
      LStyleDescriptor := TStyleManager.FindStyleDescriptor(TStyleManager.ActiveStyleForScene(Screen.ActiveForm
        as IScene));
    Result := GetThemeFromDescriptor(LStyleDescriptor);
  end;
end;

function GetNativeTheme(const AControl: TControl): Integer;
var
  LStyleDescriptor: TStyleDescription;
begin
  LStyleDescriptor := TStyleManager.GetStyleDescriptionForControl(AControl);
  Result := GetThemeFromDescriptor(LStyleDescriptor);
end;

function JBitmapToBitmap(const SrcBitmap: JBitmap; const DstBitmap: TBitmap): Boolean;
var
  BitmapInfo: AndroidBitmapInfo;
  PixelFormat: TPixelFormat;
  PixelBuffer: Pointer;
  BitmapData: TBitmapData;
begin
  Result := False;
  FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);

  if AndroidBitmap_getInfo(TJNIResolver.GetJNIEnv, TJNIResolver.JavaInstanceToID(SrcBitmap), @BitmapInfo) <> 0 then
    Exit;

  case BitmapInfo.format of
    Ord(ANDROID_BITMAP_FORMAT_RGBA_8888):
      PixelFormat := TPixelFormat.RGBA;
    Ord(ANDROID_BITMAP_FORMAT_RGB_565):
      PixelFormat := TPixelFormat.BGR_565;
    Ord(ANDROID_BITMAP_FORMAT_RGBA_4444):
      PixelFormat := TPixelFormat.BGRA4;
    Ord(ANDROID_BITMAP_FORMAT_A_8):
      PixelFormat := TPixelFormat.L;
    else
      Exit;
  end;

  if AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, TJNIResolver.JavaInstanceToID(SrcBitmap), @PixelBuffer) <> 0 then
    Exit;

  try
    DstBitmap.SetSize(BitmapInfo.Width, BitmapInfo.Height);

    if DstBitmap.Map(TMapAccess.Write, BitmapData) then
      try
        if PixelFormat = TPixelFormat.RGBA then
          Move(PixelBuffer^, BitmapData.Data^, DstBitmap.BytesPerLine * DstBitmap.Height)
        else
          ScanlineToAlphaColor(PixelBuffer, BitmapData.Data, DstBitmap.Width * DstBitmap.Height, PixelFormat);
      finally
        DstBitmap.Unmap(BitmapData);
      end;

    Result := True;
  finally
    AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, TJNIResolver.JavaInstanceToID(SrcBitmap));
  end;
end;

function BitmapToJBitmap(const SrcBitmap: TBitmap; const DstBitmap: JBitmap): Boolean;
var
  PixelBuffer: Pointer;
  BitmapData: TBitmapData;
begin
  Result := False;

  if AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, TJNIResolver.JavaInstanceToID(DstBitmap), @PixelBuffer) <> 0 then
    Exit;

  try
    if SrcBitmap.Map(TMapAccess.Read, BitmapData) then
      try
        Move(BitmapData.Data^, PixelBuffer^, SrcBitmap.BytesPerLine * SrcBitmap.Height);
      finally
        SrcBitmap.Unmap(BitmapData);
      end;

    Result := True;
  finally
    AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, TJNIResolver.JavaInstanceToID(DstBitmap));
  end;
end;

function JBitmapToSurface(const SrcBitmap: JBitmap; const DestSurface: TBitmapSurface): Boolean;
var
  PixelBuffer: Pointer;
  BitmapInfo: AndroidBitmapInfo;
  PixelFormat: TPixelFormat;
begin
  Result := False;

  FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);

  if AndroidBitmap_getInfo(TJNIResolver.GetJNIEnv, TJNIResolver.JavaInstanceToID(SrcBitmap), @BitmapInfo) <> 0 then
    Exit;

  case BitmapInfo.format of
    Ord(ANDROID_BITMAP_FORMAT_RGBA_8888):
      PixelFormat := TPixelFormat.RGBA;
    Ord(ANDROID_BITMAP_FORMAT_RGB_565):
      PixelFormat := TPixelFormat.BGR_565;
    Ord(ANDROID_BITMAP_FORMAT_RGBA_4444):
      PixelFormat := TPixelFormat.BGRA4;
    Ord(ANDROID_BITMAP_FORMAT_A_8):
      PixelFormat := TPixelFormat.L;
    else
      Exit;
  end;

  if AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, TJNIResolver.JavaInstanceToID(SrcBitmap), @PixelBuffer) <> 0 then
    Exit;
  try
    DestSurface.SetSize(BitmapInfo.Width, BitmapInfo.Height, TPixelFormat.RGBA);
    if PixelFormat = TPixelFormat.RGBA then
      Move(PixelBuffer^, DestSurface.Bits^, DestSurface.Pitch * DestSurface.Height)
    else
      ScanlineToAlphaColor(PixelBuffer, DestSurface.Bits, DestSurface.Width * DestSurface.Height, PixelFormat);

    Result := True;
  finally
    AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, TJNIResolver.JavaInstanceToID(SrcBitmap));
  end;
end;

function SurfaceToJBitmap(const SrcSurface: TBitmapSurface; const DestBitmap: JBitmap): Boolean;
var
  PixelBuffer: Pointer;
  BitmapInfo: AndroidBitmapInfo;
begin
  Result := False;

  FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);

  if AndroidBitmap_getInfo(TJNIResolver.GetJNIEnv, TJNIResolver.JavaInstanceToID(DestBitmap), @BitmapInfo) <> 0 then
    Exit;

  if AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, TJNIResolver.JavaInstanceToID(DestBitmap), @PixelBuffer) <> 0 then
    Exit;

  try
    Move(SrcSurface.Bits^, PixelBuffer^, SrcSurface.Pitch * SrcSurface.Height);

    Result := True;
  finally
    AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, TJNIResolver.JavaInstanceToID(DestBitmap));
  end;
end;

function NativeViewToSurface(const ANativeView: JView; const ASurface: TBitmapSurface): Boolean;
var
  NativeBitmap: JBitmap;
  OldCacheEnabledValue: Boolean;
  OldCacheQualityValue: Integer;
  Successful: Boolean;
begin
  CallInUIThreadAndWaitFinishing(procedure begin
    OldCacheEnabledValue := ANativeView.isDrawingCacheEnabled;
    OldCacheQualityValue := ANativeView.getDrawingCacheQuality;

    ANativeView.setDrawingCacheEnabled(True);
    ANativeView.setDrawingCacheQuality(TJView.JavaClass.DRAWING_CACHE_QUALITY_HIGH);
    try
      NativeBitmap := ANativeView.getDrawingCache;
      if NativeBitmap <> nil then
      begin
        Successful := JBitmapToSurface(NativeBitmap, ASurface);
      end;
    finally
      ANativeView.setDrawingCacheEnabled(OldCacheEnabledValue);
      ANativeView.setDrawingCacheQuality(OldCacheQualityValue);
    end;
  end);
  Result := Successful;
end;

type
  TFinishedRunnableCollector = class
  strict private class var
    FFinishedThreadCollector : TFinishedRunnableCollector;
  strict private
    FTimerService: IFMXTimerService;
    FTimerHandle: TFmxHandle;
    function SupportTimer: Boolean;
    function HasTimer: Boolean;
    procedure Start(Timeout: Integer = 2000);
  public
    constructor Create;
    class procedure Call(Timeout: Integer = 2000);
    class procedure ForceCollection;
    procedure DoTimer;
  end;

var
  ActiveJavaRunnables: TThreadList<TRunnable>;

procedure CallInUIThread(const AMethod: TMethodCallback); overload;
var
  Runnable: TRunnable;
begin
  if TThread.Current.ThreadID = MainThreadID then
    AMethod
  else
  begin
    Runnable := TRunnable.Create(AMethod);
    ActiveJavaRunnables.Add(Runnable);
    Runnable.Start;
  end;
end;

procedure CallInUIThread(const AMethod: TCallBack); overload;
var
  Runnable: TRunnable;
begin
  if TThread.Current.ThreadID = MainThreadID then
    AMethod
  else
  begin
    Runnable := TRunnable.Create(AMethod);
    ActiveJavaRunnables.Add(Runnable);
    Runnable.Start;
  end;
end;

procedure CallInUIThreadAndWaitFinishing(const AMethod: TMethodCallback); overload;
var
  Event: TEvent;
begin
  if TThread.Current.ThreadID = MainThreadID then
    AMethod
  else
  begin
    Event := TEvent.Create;
    try
      CallInUIThread(procedure
        begin
          if Assigned(AMethod) then
            AMethod;
          Event.SetEvent;
        end);
      Event.WaitFor;
    finally
      Event.Free;
    end;
  end;
end;

procedure CallInUIThreadAndWaitFinishing(const AMethod: TCallBack); overload;
var
  Event: TEvent;
begin
  if TThread.Current.ThreadID = MainThreadID then
    AMethod
  else
  begin
    Event := TEvent.Create;
    try
      CallInUIThread(procedure
        begin
          if Assigned(AMethod) then
            AMethod;
          Event.SetEvent;
        end);
      Event.WaitFor;
    finally
      Event.Free;
    end;
  end;
end;

class function TUIThreadCaller.Call<R>(const Func: TFunc<R>): R;
var
  Event: TEvent;
  Runnable: TRunnable;
  CallResult: R;
begin
  if TThread.Current.ThreadID = MainThreadID then
    CallResult := Func()
  else
  begin
    Event := TEvent.Create;
    try
      Runnable := TRunnable.Create(
        procedure
        begin
          CallResult := Func();
          Event.SetEvent;
        end);
      Runnable.Start;
      Event.WaitFor;
    finally
      Event.Free;
    end;
  end;
  Result := CallResult;
end;

class function TUIThreadCaller.Call<R>(const Func: TMethodFunc<R>): R;
var
  Event: TEvent;
  Runnable: TRunnable;
  CallResult: R;
begin
  if TThread.Current.ThreadID = MainThreadID then
    CallResult := Func()
  else
  begin
    Event := TEvent.Create;
    try
      Runnable := TRunnable.Create(
        procedure
        begin
          CallResult := Func();
          Event.SetEvent;
        end);
      Runnable.Start;
      Event.WaitFor;
    finally
      Event.Free;
    end;
  end;
  Result := CallResult;
end;

class function TUIThreadCaller.Call<A, R>(const Func: TFunc<A, R>; const Arg: A): R;
var
  Event: TEvent;
  Runnable: TRunnable;
  CallResult: R;
begin
  if TThread.Current.ThreadID = MainThreadID then
    CallResult := Func(Arg)
  else
  begin
    Event := TEvent.Create;
    try
      Runnable := TRunnable.Create(
        procedure
        begin
          CallResult := Func(Arg);
          Event.SetEvent;
        end);
      Runnable.Start;
      Event.WaitFor;
    finally
      Event.Free;
    end;
  end;
  Result := CallResult;
end;

class procedure TUIThreadCaller.Call<A, B>(const Proc: TProc<A, B>; Arg1: A; Arg2: B);
begin
  CallInUIThread(
    procedure
    begin
      Proc(Arg1, Arg2);
    end);
end;

class function TUIThreadCaller.Call<A, R>(const Func: TMethodFunc<A, R>; const Arg: A): R;
var
  Event: TEvent;
  Runnable: TRunnable;
  CallResult: R;
begin
  if TThread.Current.ThreadID = MainThreadID then
    CallResult := Func(Arg)
  else
  begin
    Event := TEvent.Create;
    try
      Runnable := TRunnable.Create(
        procedure
        begin
          CallResult := Func(Arg);
          Event.SetEvent;
        end);
      Runnable.Start;
      Event.WaitFor;
    finally
      Event.Free;
    end;
  end;
  Result := CallResult;
end;

class function TUIThreadCaller.Call<A, B, R>(const Func: TFunc<A, B, R>; const Arg1: A; const Arg2: B): R;
var
  Event: TEvent;
  Runnable: TRunnable;
  CallResult: R;
begin
  if TThread.Current.ThreadID = MainThreadID then
    CallResult := Func(Arg1, Arg2)
  else
  begin
    Event := TEvent.Create;
    try
      Runnable := TRunnable.Create(
        procedure
        begin
          CallResult := Func(Arg1, Arg2);
          Event.SetEvent;
        end);
      Runnable.Start;
      Event.WaitFor;
    finally
      Event.Free;
    end;
  end;
  Result := CallResult;
end;

class function TUIThreadCaller.Call<A, B, R>(const Func: TMethodFunc<A, B, R>; const Arg1: A; const Arg2: B): R;
var
  Event: TEvent;
  Runnable: TRunnable;
  CallResult: R;
begin
  if TThread.Current.ThreadID = MainThreadID then
    CallResult := Func(Arg1, Arg2)
  else
  begin
    Event := TEvent.Create;
    try
      Runnable := TRunnable.Create(
        procedure
        begin
          CallResult := Func(Arg1, Arg2);
          Event.SetEvent;
        end);
      Runnable.Start;
      Event.WaitFor;
    finally
      Event.Free;
    end;
  end;
  Result := CallResult;
end;

class procedure TUIThreadCaller.Call<A>(const Proc: TProc<A>; Arg: A);
begin
  CallInUIThread(
    procedure
    begin
      Proc(Arg);
    end);
end;

class procedure TUIThreadCaller.Call<A>(const Proc: TMethodProc<A>; Arg: A);
begin
  CallInUIThread(
    procedure
    begin
      Proc(Arg);
    end);
end;

class procedure TUIThreadCaller.Call<A, B>(const Proc: TMethodProc2<A,B>; Arg1: A; Arg2: B);
begin
  CallInUIThread(
    procedure
    begin
      Proc(Arg1, Arg2);
    end);
end;

class procedure TUIThreadCaller.Call<A, B, C>(const Proc: TMethodProc3<A, B, C>; Arg1: A; Arg2: B; Arg3: C);
begin
  CallInUIThread(
    procedure
    begin
      Proc(Arg1, Arg2, Arg3);
    end);
end;

class procedure TUIThreadCaller.InvokeIfNotNil<A>(const Proc: TProc<A>; const Arg: A);
begin
  CallInUIThread(procedure
  begin
    if Arg <> nil then
      Proc(Arg);
  end);
end;

class function TUIThreadCaller.CallIfNotNil<A, R>(const Func: TFunc<A, R>; const Arg: A): R;
begin
  if Arg <> nil then
    Result := Call<A, R>(Func, Arg)
  else
    Result := Default(R);
end;

class procedure TUIThreadCaller.CallWithAsyncCallback(const UIThreadProc, SyncProc: TCallback);
begin
  CallInUIThread(procedure()
    begin
      if Assigned(UIThreadProc) then
        UIThreadProc;
      TThread.Queue(nil, TThreadProcedure(SyncProc));
    end);
end;

class procedure TUIThreadCaller.ForceRunnablesCollection;
begin
  TFinishedRunnableCollector.ForceCollection;
end;

{ TRunnable }

constructor TRunnable.Create(const ACallback: TCallBack);
begin
  Create;
  FCallback := ACallback;
end;

constructor TRunnable.Create(const AMethodCallback: TMethodCallback);
begin
  Create;
  FMethodCallback := AMethodCallback;
end;

constructor TRunnable.Create;
begin
  inherited Create;
  FFinished := False;
  FActivity := TAndroidHelper.Activity;
end;

procedure TRunnable.run;
begin
  try
    if Assigned(FCallback) then
      FCallback;
    if Assigned(FMethodCallback) then
      FMethodCallback;
  finally
    FFinished := True;
    TFinishedRunnableCollector.Call;
  end;
end;

procedure TRunnable.Start;
begin
  if Assigned(FActivity) then
    FActivity.runOnUiThread(Self);
end;

{ TFinishedThreadCollector }

procedure TFinishedRunnableCollector.Start(Timeout: Integer = 2000);
begin
  if SupportTimer then
  begin
    if HasTimer then
      FTimerService.DestroyTimer(FTimerHandle);
    FTimerHandle := FTimerService.CreateTimer(Timeout, DoTimer);
  end;
end;

function TFinishedRunnableCollector.SupportTimer: Boolean;
begin
  Result := FTimerService <> nil;
end;

procedure TFinishedRunnableCollector.DoTimer;
var
  I: Integer;
  JavaThread: TRunnable;
  List: TList<TRunnable>;
begin
  try
    List := ActiveJavaRunnables.LockList;
    try
      I := 0;
      while (I < List.Count) do
      begin
        JavaThread := List[I];
        if JavaThread.Finished then
          List.Remove(JavaThread)
        else
          Inc(I);
      end;
    finally
      ActiveJavaRunnables.UnlockList;
    end;
  finally
    if SupportTimer then
    begin
      FTimerService.DestroyTimer(FTimerHandle);
      FTimerHandle := 0;
    end;
  end;
end;

class procedure TFinishedRunnableCollector.ForceCollection;
begin
  if FFinishedThreadCollector <> nil then
    FFinishedThreadCollector.DoTimer;
end;

function TFinishedRunnableCollector.HasTimer: Boolean;
begin
  Result := FTimerHandle <> 0;
end;

constructor TFinishedRunnableCollector.Create;
begin
  TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService);
  FTimerHandle := 0;
end;

class procedure TFinishedRunnableCollector.Call(Timeout: Integer);
begin
  if not Assigned(FFinishedThreadCollector) then
    FFinishedThreadCollector := TFinishedRunnableCollector.Create;

  FFinishedThreadCollector.Start;
end;

function CreateJavaStringArray(const ASource: array of const): TJavaObjectArray<JString>;
var
  I: Integer;
  JavaString: JString;
begin
  Result := TJavaObjectArray<JString>.Create(Length(ASource));
  for I := 0 to Length(ASource) - 1 do
    case ASource[I].vType of
      vtUnicodeString:
        Result[I] := StringToJString(string(ASource[I].VWideString));

      vtWideChar:
        Result[I] := StringToJString(ASource[I].VWideChar);

      vtInteger:
        Result[I] := StringToJString(ASource[I].VInteger.ToString);

      vtInt64:
        Result[I] := StringToJString(ASource[I].VInt64^.ToString);

      vtInterface:
        if Supports(IInterface(ASource[I].VInterface), JString, JavaString) then
          Result[I] := JavaString;
    else
      raise EArgumentException.CreateFmt(SCannotConvertDelphiArrayToJStringArray, [I]);
    end;
end;

initialization
  ActiveJavaRunnables := TThreadList<TRunnable>.Create;

finalization
  FreeAndNil(ActiveJavaRunnables);
end.
