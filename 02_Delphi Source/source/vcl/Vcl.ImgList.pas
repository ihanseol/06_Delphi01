{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Vcl.ImgList;

{$HPPEMIT LEGACYHPP}
{$R-,T-,H+,X+}

interface

uses
{$IF DEFINED(CLR)}
  System.ComponentModel.Design.Serialization,
{$ENDIF}
  Winapi.Windows, System.Classes, Vcl.Graphics, Winapi.CommCtrl,
  System.ImageList, System.UITypes;

type

{ TChangeLink }

  TCustomImageList = class;

  TChangeLink = class(TImageLink)
  private
    function GetSender: TCustomImageList; inline;
    procedure SetSender(const Value: TCustomImageList); inline;
  public
    constructor Create; override;
    property Sender: TCustomImageList read GetSender write SetSender;
  end;

{ TImageListHandle }

{$IF DEFINED(CLR)}
  TImageListHandle = class(TResHandleWrapper)
  private
    FShareImages: Boolean;
  strict protected
    procedure Finalize; override;
  public
    class operator Implicit(AValue: TImageListHandle): HIMAGELIST;
    class operator Equal(Left: TImageListHandle; Right: HIMAGELIST): Boolean;
    class operator NotEqual(Left: TImageListHandle; Right: HIMAGELIST): Boolean;
    property ShareImages: Boolean read FShareImages write FShareImages default False;
  end;
{$ELSE}
  TImageListHandle = HImageList;
{$ENDIF}

{ TCustomImageList }

  TDrawingStyle = (dsFocus, dsSelected, dsNormal, dsTransparent);
  TImageType = (itImage, itMask);
  TResType = (rtBitmap, rtCursor, rtIcon);
  TOverlay = 0..14;
  TLoadResource = (lrDefaultColor, lrDefaultSize, lrFromFile,
    lrMap3DColors, lrTransparent, lrMonoChrome);
  TLoadResources = set of TLoadResource;
  TImageIndex = System.UITypes.TImageIndex deprecated 'Use System.UITypes.TImageIndex';
  TColorDepth = (cdDefault, cdDeviceDependent, cd4Bit, cd8Bit, cd16Bit, cd24Bit, cd32Bit);
  TDisabledStyle = (diGrayscale, diMonochrome, diDesaturate, diDesaturateBlend25, diDesaturateBlend50);

  {$IFDEF CLR}[RootDesignerSerializerAttribute('', '', False)]{$ENDIF}
  TCustomImageList = class(TBaseImageList)
  private
    FHeight: Integer;
    FWidth: Integer;
    FAllocBy: Integer;
    FHandle: TImageListHandle;
    FDrawingStyle: TDrawingStyle;
    FMasked: Boolean;
    FShareImages: Boolean;
    FImageType: TImageType;
    FBkColor: TColor;
    FBlendColor: TColor;
    FGrayscaleFactor: Byte;
    FBitmap: TBitmap;
    FMonoBitmap: TBitmap;
    FGrayscaleBitmap: TBitmap;
    FOnChange: TNotifyEvent;
    FColorDepth: TColorDepth;
    FStoreBitmap: Boolean;
    FDisabledStyle: TDisabledStyle;
    procedure InitBitmap;
    procedure CopyImages(Value: HImageList; Index: Integer = -1);
    procedure CopyFromImageList(Value: TCustomImageList; Index: Integer = -1; Disabled: Boolean = False);
    procedure CreateImageList;
    function Equal(IL: TCustomImageList): Boolean;
    procedure FreeHandle;
    function GetBitmapHandle(Bitmap: HBITMAP): HBITMAP;
    function GetBkColor: TColor;
    function GetHandle: HImageList;
    function GetImageHandle(Image, ImageLocal: TBitmap): HBITMAP;
    procedure InsertImage(Index: Integer; Image, Mask: TBitmap; MaskColor: TColor);
    procedure SetBkColor(Value: TColor);
    procedure SetDrawingStyle(Value: TDrawingStyle);
    procedure SetDisabledStyle(Value: TDisabledStyle);
    procedure SetHandle(Value: HImageList);
    procedure SetHeight(Value: Integer);
    procedure SetNewDimensions(Value: HImageList);
    procedure SetScaled(Value: Boolean);
    procedure SetShareImages(Value: Boolean);
    procedure SetWidth(Value: Integer);
    procedure SetColorDepth(Value: TColorDepth);
    procedure ReadD2Stream(Stream: TStream);
    procedure ReadD3Stream(Stream: TStream);
{$IF DEFINED(CLR)}
    function InternalGetInstRes(Instance: THandle; ResType: TResType;
      Name: string; ResID: DWORD; Width: Integer; LoadFlags: TLoadResources;
      MaskColor: TColor): Boolean;
{$ELSE}
    function InternalGetInstRes(Instance: THandle; ResType: TResType;
      Name: PChar; Width: Integer; LoadFlags: TLoadResources;
      MaskColor: TColor): Boolean;
{$ENDIF}
  protected
    FScaling: Boolean;
    FScaled: Boolean;
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer;
      Style: Cardinal; Enabled: Boolean = True); virtual;
    function GetCount: Integer; override;
    procedure GetImages(Index: Integer; Image, Mask: TBitmap);
    procedure HandleNeeded;
    procedure Initialize; virtual;
    procedure ReadData(Stream: TStream); virtual;
    procedure WriteData(Stream: TStream); virtual;
    procedure DoChange; override;
    property StoreBitmap: Boolean read FStoreBitmap write FStoreBitmap;
    property Scaled: Boolean read FScaled write SetScaled;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Add(Image, Mask: TBitmap): Integer;
    function AddIcon(Image: TIcon): Integer;
    function AddImage(Value: TCustomImageList; Index: Integer): Integer;
    function AddDisabledImage(Value: TCustomImageList; Index: Integer): Integer;
    procedure AddImages(Value: TCustomImageList);
    procedure AddDisabledImages(Value: TCustomImageList);
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Draw(Canvas: TCanvas; X, Y, Index: Integer;
      Enabled: Boolean = True); overload;
    procedure Draw(Canvas: TCanvas; X, Y, Index: Integer;
      ADrawingStyle: TDrawingStyle; AImageType: TImageType;
      Enabled: Boolean = True); overload;
    procedure DrawOverlay(Canvas: TCanvas; X, Y: Integer;
      ImageIndex: Integer; Overlay: TOverlay; Enabled: Boolean = True); overload;
    procedure DrawOverlay(Canvas: TCanvas; X, Y: Integer;
      ImageIndex: Integer; Overlay: TOverlay; ADrawingStyle: TDrawingStyle;
      AImageType: TImageType; Enabled: Boolean = True); overload;
    function FileLoad(ResType: TResType; const Name: string;
      MaskColor: TColor): Boolean;
    function GetBitmap(Index: Integer; Image: TBitmap): Boolean;
    function GetHotSpot: TPoint; virtual;
    procedure GetIcon(Index: Integer; Image: TIcon); overload;
    procedure GetIcon(Index: Integer; Image: TIcon; ADrawingStyle: TDrawingStyle;
      AImageType: TImageType); overload;
    function GetImageBitmap: HBITMAP;
    function GetMaskBitmap: HBITMAP;
    function GetResource(ResType: TResType; const Name: string;
      Width: Integer; LoadFlags: TLoadResources; MaskColor: TColor): Boolean;
    function GetInstRes(Instance: THandle; ResType: TResType; const Name: string;
      Width: Integer; LoadFlags: TLoadResources; MaskColor: TColor): Boolean; overload;
    function GetInstRes(Instance: THandle; ResType: TResType; ResID: DWORD;
      Width: Integer; LoadFlags: TLoadResources; MaskColor: TColor): Boolean; overload;
    function HandleAllocated: Boolean; inline;
    procedure Insert(Index: Integer; Image, Mask: TBitmap);
    procedure InsertIcon(Index: Integer; Image: TIcon);
    procedure InsertMasked(Index: Integer; Image: TBitmap; MaskColor: TColor);
    procedure Move(CurIndex, NewIndex: Integer);
    function Overlay(ImageIndex: Integer; Overlay: TOverlay): Boolean; virtual;
    procedure RegisterChanges(Value: TChangeLink);
    function ResourceLoad(ResType: TResType; const Name: string;
      MaskColor: TColor): Boolean;
    function ResInstLoad(Instance: THandle; ResType: TResType;
      const Name: string; MaskColor: TColor): Boolean;
    procedure Replace(Index: Integer; Image, Mask: TBitmap);
    procedure ReplaceIcon(Index: Integer; Image: TIcon);
    procedure ReplaceMasked(Index: Integer; NewImage: TBitmap; MaskColor: TColor);
    procedure SetSize(AWidth, AHeight: Integer);
    procedure UnRegisterChanges(Value: TChangeLink);
    property ColorDepth: TColorDepth read FColorDepth write SetColorDepth default cdDeviceDependent;
    property Handle: HImageList read GetHandle write SetHandle;
    property Scaling: Boolean read FScaling;
  public
    /// <summary>
    /// Defines that name is available for image.
    /// </summary>
    function IsImageNameAvailable: Boolean; virtual;
    /// <summary>
    /// Defines that image list is scalable
    /// </summary>
    function IsScaled: Boolean; virtual;
    /// <summary>
    /// Get image index from specific name.
    /// </summary>
    function GetIndexByName(const AName: TImageName): System.UITypes.TImageIndex; virtual;
    /// <summary>
    /// Get image name from specific index.
    /// </summary>
    function GetNameByIndex(AIndex: System.UITypes.TImageIndex): TImageName; virtual;
    /// <summary>
    /// Check image name with specific index and if name is not the same then get new image index with this name
    /// </summary>
    procedure CheckIndexAndName(var AIndex: System.UITypes.TImageIndex; var AName: TImageName);
    property AllocBy: Integer read FAllocBy write FAllocBy default 4;
    property BlendColor: TColor read FBlendColor write FBlendColor default clNone;
    property GrayscaleFactor: Byte read FGrayscaleFactor write FGrayscaleFactor default 0;
    property BkColor: TColor read GetBkColor write SetBkColor default clNone;
    property DrawingStyle: TDrawingStyle read FDrawingStyle write SetDrawingStyle default dsNormal;
    property DisabledStyle: TDisabledStyle read FDisabledStyle write SetDisabledStyle default diDesaturateBlend25;
    property Height: Integer read FHeight write SetHeight default 16;
    property ImageType: TImageType read FImageType write FImageType default itImage;
    property Masked: Boolean read FMasked write FMasked default True;
    property ShareImages: Boolean read FShareImages write SetShareImages default False;
    property Width: Integer read FWidth write SetWidth default 16;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  function GetRGBFromColor(Value: TColor): DWORD;
  function GetColorFromRGB(Value: DWORD): TColor;

implementation

uses
{$IF DEFINED(CLR)}
  WinUtils, Types, System.Runtime.InteropServices, System.Security.Permissions, System.Security,
{$ENDIF}
  System.Types,
  System.SysUtils, Vcl.Forms, Winapi.ActiveX, Vcl.Consts, Vcl.Themes, Vcl.Controls, Vcl.GraphUtil;

{ TCustomImageList }

const
  DrawingStyles: array[TDrawingStyle] of Longint = (ILD_FOCUS, ILD_SELECTED,
    ILD_NORMAL, ILD_TRANSPARENT);
  Images: array[TImageType] of Longint = (0, ILD_MASK);

  ColorDepthToILConst: array[Low(TColorDepth)..High(TColorDepth)] of LongInt = (ILC_COLOR,
    ILC_COLORDDB, ILC_COLOR4, ILC_COLOR8, ILC_COLOR16, ILC_COLOR24, ILC_COLOR32);
  ColorDepthToPixelFormat: array [Low(TColorDepth)..High(TColorDepth)] of TPixelFormat =
  (pf4bit, pfDevice, pf4bit, pf8bit, pf16bit, pf24bit, pf32bit);

function GetRGBFromColor(Value: TColor): DWORD;
begin
  Result := ColorToRGB(Value);
  case Result of
    clNone: Result := CLR_NONE;
    clDefault: Result := CLR_DEFAULT;
  end;
end;

function GetColorFromRGB(Value: DWORD): TColor;
begin
  case Value of
    CLR_NONE: Result := clNone;
    CLR_DEFAULT: Result := clDefault;
  else
    Result := TColor(Value);
  end;
end;

{ TImageListHandle }

{$IF DEFINED(CLR)}
procedure TImageListHandle.Finalize;
begin
  if (Handle <> 0) and not ShareImages then
    ImageList_Destroy(Handle);
  Handle := 0;
  inherited;
end;

class operator TImageListHandle.Implicit(AValue: TImageListHandle): HIMAGELIST;
begin
  Result := AValue.Handle;
end;

class operator TImageListHandle.Equal(Left: TImageListHandle; Right: HIMAGELIST): Boolean;
begin
  Result := Left.Handle = Right;
end;

class operator TImageListHandle.NotEqual(Left: TImageListHandle; Right: HIMAGELIST): Boolean;
begin
  Result := Left.Handle <> Right;
end;
{$ENDIF}

{ TCustomImageList }

constructor TCustomImageList.Create(AOwner: TComponent);
const
  cDefaultSize = 16;
begin
  inherited Create(AOwner);
{$IF DEFINED(CLR)}
  FHandle := TImageListHandle.Create;
{$ENDIF}
  FStoreBitmap := True;
  FWidth := cDefaultSize;
  FHeight := cDefaultSize;
  Initialize;
end;

constructor TCustomImageList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited Create(nil);
{$IF DEFINED(CLR)}
  FHandle := TImageListHandle.Create;
{$ENDIF}
  FStoreBitmap := True;
  FWidth := AWidth;
  FHeight := AHeight;
  Initialize;
end;

destructor TCustomImageList.Destroy;
begin
  FOnChange := nil;
  FBitmap.Free;
  FreeHandle;
{$IF DEFINED(CLR)}
  System.GC.SuppressFinalize(FHandle);
{$ENDIF}
  FMonoBitmap.Free;
  FGrayscaleBitmap.Free;
  inherited Destroy;
end;

procedure TCustomImageList.Initialize;
const
  MaxSize = 32768;
begin
  if (Height < 1) or (Height > MaxSize) or (Width < 1) then
    raise EInvalidOperation.Create(SInvalidImageSize);
  AllocBy := 4;
  Masked := True;
  DrawingStyle := dsNormal;
  ImageType := itImage;
  FBkColor := clNone;
  FBlendColor := clNone;
  FGrayscaleFactor := 0;
  FColorDepth := cdDeviceDependent;
  FBitmap := TBitmap.Create;
  FDisabledStyle := diDesaturateBlend25;
  InitBitmap;
end;

function TCustomImageList.HandleAllocated: Boolean;
begin
  Result := FHandle <> 0;
end;

procedure TCustomImageList.HandleNeeded;
begin
  if FHandle = 0 then CreateImageList;
end;

procedure TCustomImageList.InitBitmap;
var
  ScreenDC: HDC;
begin
  ScreenDC := GetDC(0);
  try
    with FBitmap do
    begin

      Handle := CreateCompatibleBitmap(ScreenDC, Self.Width, Self.Height);
      Canvas.Brush.Color := clBlack;
      Canvas.FillRect(Rect(0, 0, Width, Height));
    end;
  finally
    ReleaseDC(0, ScreenDC);
  end;

  FreeAndNil(FMonoBitmap);
  FreeAndNil(FGrayscaleBitmap);
end;

procedure TCustomImageList.SetNewDimensions(Value: HImageList);
var
  AHeight, AWidth: Integer;
begin
  AWidth := Width;
  AHeight := Height;
  ImageList_GetIconSize(Value, AWidth, AHeight);
  FWidth := AWidth;
  FHeight := AHeight;
  InitBitmap;
end;

procedure TCustomImageList.SetScaled(Value: Boolean);
begin
  if Value <> Scaled then
  begin
    FScaled := Value;
    Change;
  end;
end;

procedure TCustomImageList.SetShareImages(Value: Boolean);
begin
  if Value <> FShareImages then
  begin
    FShareImages := Value;
{$IF DEFINED(CLR)}
    FHandle.ShareImages := Value;
{$ENDIF}
  end;
end;

procedure TCustomImageList.SetWidth(Value: Integer);
begin
  if Value <> Width then
  begin
    FWidth := Value;
    if HandleAllocated then
      ImageList_SetIconSize(FHandle, Width, Height);
    Clear;
    InitBitmap;
    Change;
  end;
end;

procedure TCustomImageList.SetHeight(Value: Integer);
begin
  if Value <> Height then
  begin
    FHeight := Value;
    if HandleAllocated then
      ImageList_SetIconSize(FHandle, Width, Height);
    Clear;
    InitBitmap;
    Change;
  end;
end;

procedure TCustomImageList.SetHandle(Value: HImageList);
begin
  FreeHandle;
  if Value <> 0 then
  begin
    SetNewDimensions(Value);
{$IF DEFINED(CLR)}
    FHandle.Handle := Value;
{$ELSE}
    FHandle := Value;
{$ENDIF}
    Change;
  end;
end;

function TCustomImageList.GetBitmapHandle(Bitmap: HBITMAP): HBITMAP;
begin
  if Bitmap <> 0 then
    Result := Bitmap else
    Result := FBitmap.Handle;
end;

function TCustomImageList.GetHandle: HImageList;
begin
  HandleNeeded;
  Result := FHandle;
end;

function TCustomImageList.GetImageHandle(Image, ImageLocal: TBitmap): HBITMAP;
begin
  if Image <> nil then
    if Image.PixelFormat = ColorDepthToPixelFormat[ColorDepth] then
      Result := Image.Handle
    else
    begin
      ImageLocal.SetSize(Image.Width, Image.Height);
      ImageLocal.PixelFormat := ColorDepthToPixelFormat[ColorDepth];
      ImageLocal.Canvas.Draw(0,0, Image);

      Result := ImageLocal.Handle;
    end
  else Result := FBitmap.Handle;
end;

procedure TCustomImageList.FreeHandle;
begin
  if HandleAllocated and not ShareImages then
    ImageList_Destroy(Handle);
{$IF DEFINED(CLR)}
  FHandle.Handle := 0;
{$ELSE}
  FHandle := 0;
{$ENDIF}
  Change;
end;

procedure TCustomImageList.CreateImageList;
const
  Mask: array[Boolean] of Longint = (0, ILC_MASK);
begin
{$IF DEFINED(CLR)}
  FHandle.Handle := ImageList_Create(Width, Height, ColorDepthToILConst[FColorDepth] or Mask[Masked],
    AllocBy, AllocBy);
{$ELSE}
  FHandle := ImageList_Create(Width, Height, ColorDepthToILConst[FColorDepth] or Mask[Masked],
    AllocBy, AllocBy);
{$ENDIF}
  if not HandleAllocated then raise EInvalidOperation.Create(SInvalidImageList);
  if FBkColor <> clNone then BkColor := FBkColor;
end;

function TCustomImageList.GetImageBitmap: HBITMAP;
var
  Info: TImageInfo;
begin
  if (Count > 0) and ImageList_GetImageInfo(Handle, 0, Info) then
    Result := Info.hbmImage
  else Result := 0;
end;

function TCustomImageList.GetMaskBitmap: HBITMAP;
var
  Info: TImageInfo;
begin
  if (Count > 0) and ImageList_GetImageInfo(Handle, 0, Info) then
    Result := Info.hbmMask
  else Result := 0;
end;

function TCustomImageList.Add(Image, Mask: TBitmap): Integer;
var
  ImageLocal, MaskLocal: TBitmap;
begin
  ImageLocal := TBitmap.Create;
  try
    MaskLocal := TBitmap.Create;
    try
      HandleNeeded;
      Result := ImageList_Add(FHandle, GetImageHandle(Image, ImageLocal),
        GetImageHandle(Mask, MaskLocal));
    finally
      MaskLocal.Free;
    end;
  finally
    ImageLocal.Free;
  end;
  Change;
end;

function TCustomImageList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
var
  ImageLocal: TBitmap;
begin
  ImageLocal := TBitmap.Create;
  try
    if Masked and (MaskColor <> -1) then
    begin
      with TBitmap.Create do
      try
        Assign(Image);
        TransparentColor := MaskColor;
        Self.HandleNeeded;
        Result := ImageList_Add(Self.FHandle, GetImageHandle(Image, ImageLocal),
          GetBitmapHandle(MaskHandle));
      finally
        Free;
      end;
    end
    else Result := ImageList_Add(Handle, GetImageHandle(Image, ImageLocal), 0);
  finally
    ImageLocal.Free;
  end;
  Change;
end;

function TCustomImageList.AddIcon(Image: TIcon): Integer;
begin
  if Image = nil then
    Result := Add(nil, nil)
  else
    Result := ImageList_AddIcon(Handle, Image.Handle);
  Change;
end;

function TCustomImageList.GetBitmap(Index: Integer; Image: TBitmap): Boolean;
begin
  Result := (Image <> nil) and HandleAllocated and (Index > -1) and (Index < Count);
  if Result then
    with Image do
    begin
      Height := FHeight;
      Width := FWidth;
      Draw(Canvas, 0, 0, Index);
    end;
end;

procedure TCustomImageList.GetIcon(Index: Integer; Image: TIcon);
begin
  GetIcon(Index, Image, DrawingStyle, ImageType);
end;

procedure TCustomImageList.GetIcon(Index: Integer; Image: TIcon;
  ADrawingStyle: TDrawingStyle; AImageType: TImageType);
begin
  if (Image <> nil) and HandleAllocated then
    Image.Handle := ImageList_GetIcon(Handle, Index,
      DrawingStyles[ADrawingStyle] or Images[AImageType]);
end;

function TCustomImageList.GetCount: Integer;
begin
  if HandleAllocated then Result := ImageList_GetImageCount(Handle)
  else Result := 0;
end;

procedure TCustomImageList.Replace(Index: Integer; Image, Mask: TBitmap);
var
  ImageLocal, MaskLocal: TBitmap;
begin
  ImageLocal := TBitmap.Create;
  try
    MaskLocal := TBitmap.Create;
    try
      if HandleAllocated and not ImageList_Replace(Handle, Index,
        GetImageHandle(Image, ImageLocal), GetImageHandle(Mask, MaskLocal)) then
          raise EInvalidOperation.Create(SReplaceImage);
    finally
      MaskLocal.Free;
    end;
  finally
    ImageLocal.Free;
  end;
  Change;
end;

procedure TCustomImageList.ReplaceMasked(Index: Integer; NewImage: TBitmap; MaskColor: TColor);
var
  TempIndex: Integer;
  Image, Mask: TBitmap;
begin
  if HandleAllocated then
  begin
    TempIndex := AddMasked(NewImage, MaskColor);
    if TempIndex <> -1 then
    try
      Image := TBitmap.Create;
      try
        with Image do
        begin
          Height := FHeight;
          Width := FWidth;
        end;
        Mask := TBitmap.Create;
        try
          with Mask do
          begin
            Monochrome := True;
            Height := FHeight;
            Width := FWidth;
          end;
          ImageList_Draw(Handle, TempIndex, Image.Canvas.Handle, 0, 0, ILD_NORMAL);
          ImageList_Draw(Handle, TempIndex, Mask.Canvas.Handle, 0, 0, ILD_MASK);
          if not ImageList_Replace(Handle, Index, Image.Handle, Mask.Handle) then
            raise EInvalidOperation.Create(SReplaceImage);
        finally
          Mask.Free;
        end;
      finally
        Image.Free;
      end;
    finally
      Delete(TempIndex);
    end
    else raise EInvalidOperation.Create(SReplaceImage);
  end;
  Change;
end;

procedure TCustomImageList.ReplaceIcon(Index: Integer; Image: TIcon);
begin
  if HandleAllocated then
    if Image = nil then
      Replace(Index, nil, nil)
    else
      if ImageList_ReplaceIcon(Handle, Index, Image.Handle) = -1 then
        raise EInvalidOperation.Create(SReplaceImage);
  Change;
end;

procedure TCustomImageList.Delete(Index: Integer);
begin
  if Index >= Count then
    raise EInvalidOperation.Create(SImageIndexError);
  if HandleAllocated then
    ImageList_Remove(Handle, Index);
  Change;
end;

procedure TCustomImageList.Clear;
begin
  Delete(-1);
end;

procedure TCustomImageList.SetBkColor(Value: TColor);
begin
  if HandleAllocated then 
    ImageList_SetBkColor(FHandle, GetRGBFromColor(Value))
  else FBkColor := Value;
  Change;
end;

procedure TCustomImageList.SetColorDepth(Value: TColorDepth);
begin
  if FColorDepth <> Value then
  begin
    BeginUpdate; // Free handle generates a OnChange event, but the event handler shouldn't recreate the handle.
    try
      FreeHandle;
      FColorDepth := Value;
      CreateImageList;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TCustomImageList.SetSize(AWidth, AHeight: Integer);
begin
  if (AHeight <> Height) or (AWidth <> Width) then
  begin
    BeginUpdate;
    try
      FHeight := AHeight;
      FWidth := AWidth;
      if HandleAllocated then
        ImageList_SetIconSize(FHandle, Width, Height);
      Clear;
      InitBitmap;
    finally
      EndUpdate;
    end;
  end;
end;

function TCustomImageList.GetBkColor: TColor;
begin
  if HandleAllocated then Result := GetColorFromRGB(ImageList_GetBkColor(Handle))
  else Result := FBkColor;
end;

procedure TCustomImageList.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer;
  Style: Cardinal; Enabled: Boolean);
const
  ROP_DSPDxax = $00E20746;
var
  R: TRect;
  DestDC, SrcDC: HDC;
  P: Pointer;

  procedure BitmapGrayscale(ABitmap: TBitmap);
  var
    X: Integer;
    Y: Integer;
    Gray: Byte;
    Pixel: PRGBQuad;
  begin
    for Y := 0 to ABitmap.Height - 1 do
    begin
      Pixel := ABitmap.ScanLine[Y];
      for X := 0 to ABitmap.Width - 1 do
      begin
        Gray := Round(((0.299 * Pixel.rgbRed) + (0.587 * Pixel.rgbGreen) + (0.114 * Pixel.rgbBlue)) * (FGrayscaleFactor / 255));
        Pixel.rgbRed := Gray;
        Pixel.rgbGreen := Gray;
        Pixel.rgbBlue := Gray;
        Inc(Pixel);
      end;
    end;
  end;

  procedure DrawDisabled(DC: HDC);
  var
    LImageParams: TImageListDrawParams;
  begin
    ZeroMemory(@LImageParams, SizeOf(LImageParams));
    LImageParams.cbSize := SizeOf(LImageParams);
    LImageParams.himl := Handle;
    LImageParams.i := Index;
    LImageParams.hdcDst := DC;
    LImageParams.x := X;
    LImageParams.y := Y;
    if Style = 0 then
      case DisabledStyle of
      diDesaturate:        ;
      diDesaturateBlend25: LImageParams.fStyle := ILD_BLEND25;
      diDesaturateBlend50: LImageParams.fStyle := ILD_BLEND50;
      end
    else
      LImageParams.fStyle := Style;
    LImageParams.fState := ILS_SATURATE;
    if BkColor <> clNone then
      LImageParams.rgbBk := GetRGBFromColor(BkColor);
    if BlendColor <> clNone then
      LImageParams.rgbFg := GetRGBFromColor(BlendColor);
    ImageList_DrawIndirect(@LImageParams);
  end;

begin
  if HandleAllocated then
  begin
    if Enabled then
      ImageList_DrawEx(Handle, Index, Canvas.Handle, X, Y, 0, 0,
        GetRGBFromColor(BkColor), GetRGBFromColor(BlendColor), Style)
    else
    begin
      if FGrayscaleFactor <> 0 then
      begin
        if FGrayscaleBitmap = nil then
        begin
          FGrayscaleBitmap := TBitmap.Create;
          with FGrayscaleBitmap do
          begin
            PixelFormat := pf32Bit;
            Width := Self.Width;
            Height := Self.Height;
            HandleType := bmDIB;
            IgnorePalette := True;
            AlphaFormat := afPremultiplied;
          end;
        end;
        // Clear it always
        P := FGrayscaleBitmap.ScanLine[FGrayscaleBitmap.Height - 1];
        FillChar(P^, BytesPerScanLine(FGrayscaleBitmap.Width, 32, 32) * FGrayscaleBitmap.Height, 0);

        // Draw the image into the bitmap
        ImageList_DrawEx(Handle, Index, FGrayscaleBitmap.Canvas.Handle, 0, 0, 0, 0,
          CLR_NONE, CLR_NONE, ILD_TRANSPARENT);

        BitmapGrayscale(FGrayscaleBitmap);

        Canvas.Draw(X, Y, FGrayscaleBitmap);
      end
      else
      if (DisabledStyle in [diDesaturate, diDesaturateBlend25, diDesaturateBlend50]) and
         (ColorDepth in [cdDeviceDependent, cd32Bit]) and TStyleManager.SystemStyle.Enabled then
        DrawDisabled(Canvas.Handle)
      else
      begin
        if FMonoBitmap = nil then
        begin
          FMonoBitmap := TBitmap.Create;
          with FMonoBitmap do
          begin
            Monochrome := True;
            Width := Self.Width;
            Height := Self.Height;
          end;
        end;
        // Store masked version of image temporarily in FBitmap
        FMonoBitmap.Canvas.Brush.Color := clWhite;
        FMonoBitmap.Canvas.FillRect(Rect(0, 0, Self.Width, Self.Height));
        ImageList_DrawEx(Handle, Index, FMonoBitmap.Canvas.Handle, 0, 0, 0, 0,
          CLR_NONE, 0, ILD_NORMAL);
        R := Rect(X, Y, X+Width, Y+Height);
        SrcDC := FMonoBitmap.Canvas.Handle;
        // Convert Black to clBtnHighlight
        Canvas.Brush.Color := clBtnHighlight;
        DestDC := Canvas.Handle;
        Winapi.Windows.SetTextColor(DestDC, clWhite);
        Winapi.Windows.SetBkColor(DestDC, clBlack);
        BitBlt(DestDC, X+1, Y+1, Width, Height, SrcDC, 0, 0, ROP_DSPDxax);
        // Convert Black to clBtnShadow
        Canvas.Brush.Color := clBtnShadow;
        DestDC := Canvas.Handle;
        Winapi.Windows.SetTextColor(DestDC, clWhite);
        Winapi.Windows.SetBkColor(DestDC, clBlack);
        BitBlt(DestDC, X, Y, Width, Height, SrcDC, 0, 0, ROP_DSPDxax);
      end;
    end;
  end;
end;

procedure TCustomImageList.Draw(Canvas: TCanvas; X, Y, Index: Integer;
  Enabled: Boolean);
begin
  Draw(Canvas, X, Y, Index, DrawingStyle, ImageType, Enabled);
end;

procedure TCustomImageList.Draw(Canvas: TCanvas; X, Y, Index: Integer;
  ADrawingStyle: TDrawingStyle; AImageType: TImageType; Enabled: Boolean);
begin
  if HandleAllocated then
    DoDraw(Index, Canvas, X, Y, DrawingStyles[ADrawingStyle] or
      Images[AImageType], Enabled);
end;

procedure TCustomImageList.DrawOverlay(Canvas: TCanvas; X, Y: Integer;
  ImageIndex: Integer; Overlay: TOverlay; Enabled: Boolean);
begin
  DrawOverlay(Canvas, X, Y, ImageIndex, Overlay, dsNormal, itImage, Enabled);
end;

procedure TCustomImageList.DrawOverlay(Canvas: TCanvas; X, Y: Integer;
  ImageIndex: Integer; Overlay: TOverlay; ADrawingStyle: TDrawingStyle;
  AImageType: TImageType; Enabled: Boolean);
var
  Index: Integer;
begin
  if HandleAllocated then
  begin
    Index := IndexToOverlayMask(Overlay + 1);
    DoDraw(ImageIndex, Canvas, X, Y, DrawingStyles[ADrawingStyle] or
      Images[AImageType] or ILD_OVERLAYMASK and Index, Enabled);
  end;
end;

function TCustomImageList.Overlay(ImageIndex: Integer; Overlay: TOverlay): Boolean;
begin
  if HandleAllocated then
    Result := ImageList_SetOverlayImage(Handle, ImageIndex, Overlay + 1)
  else Result := False;
end;

procedure TCustomImageList.CopyImages(Value: HImageList; Index: Integer = -1);
var
  I: Integer;
  Image, Mask: TBitmap;
  LRect: TRect;
  LCount: Integer;

  procedure AddBitmap(IIndex: Integer);
  begin
    Image.Canvas.FillRect(LRect);
    if Image.PixelFormat = pf32bit then
      Image.AlphaFormat := afPremultiplied;
    ImageList_Draw(Value, IIndex, Image.Canvas.Handle, 0, 0, ILD_NORMAL);
    if (Image.PixelFormat = pf32bit) and CheckAlpha(Image) then
      Image.AlphaFormat := afIgnored;
    Mask.Canvas.FillRect(LRect);
    ImageList_Draw(Value, IIndex, Mask.Canvas.Handle, 0, 0, ILD_MASK);
    Add(Image, Mask);
  end;

begin
  LRect := Rect(0, 0, Width, Height);
  BeginUpdate;
  try
    Image := TBitmap.Create;
    try
      Image.PixelFormat := ColorDepthToPixelFormat[ColorDepth];
      Image.SetSize(FWidth, FHeight);
      if Image.PixelFormat = pf32bit then
        InitAlpha(Image, 0);
      Mask := TBitmap.Create;
      try
        Mask.Monochrome := True;
        Mask.Height := FHeight;
        Mask.Width := FWidth;
        LCount := ImageList_GetImageCount(Value);
        if (Index >= 0) and (Index < LCount) then
          AddBitmap(Index)
        else
        if Index = -1 then
          for I := 0 to LCount - 1 do
            AddBitmap(I);
      finally
        Mask.Free;
      end;
    finally
      Image.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomImageList.CopyFromImageList(Value: TCustomImageList; Index: Integer = -1; Disabled: Boolean = False);
var
  I: Integer;
  Image, Mask: TBitmap;
  LRect: TRect;

  procedure AddBitmap(IIndex: Integer);
  begin
    Image.Canvas.FillRect(LRect);
    if Image.PixelFormat = pf32bit then
      Image.AlphaFormat := afPremultiplied;
    Value.Draw(Image.Canvas, 0, 0, IIndex, not Disabled);
    if (Image.PixelFormat = pf32bit) and CheckAlpha(Image) then
      Image.AlphaFormat := afIgnored;
    Mask.Canvas.FillRect(LRect);
    ImageList_Draw(Value.Handle, IIndex, Mask.Canvas.Handle, 0, 0, ILD_MASK);
    Add(Image, Mask);
  end;

begin
  LRect := Rect(0, 0, Width, Height);
  BeginUpdate;
  try
    Image := TBitmap.Create;
    try
      Image.PixelFormat := ColorDepthToPixelFormat[ColorDepth];
      Image.SetSize(FWidth, FHeight);
      if Image.PixelFormat = pf32bit then
        InitAlpha(Image, 0);
      Mask := TBitmap.Create;
      try
        Mask.Monochrome := True;
        Mask.Height := FHeight;
        Mask.Width := FWidth;
        if (Index >= 0) and (Index < Value.Count) then
          AddBitmap(Index)
        else
        if Index = -1 then
          for I := 0 to Value.Count - 1 do
            AddBitmap(I);
      finally
        Mask.Free;
      end;
    finally
      Image.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomImageList.GetImages(Index: Integer; Image, Mask: TBitmap);
var
  R: TRect;
begin
  R := Rect(0, 0, Width, Height);
  with Image.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(R);
    ImageList_Draw(Self.Handle, Index, Handle, 0, 0, ILD_NORMAL);
  end;
  with Mask.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(R);
    ImageList_Draw(Self.Handle, Index, Handle, 0, 0, ILD_MASK);
  end;
end;

procedure TCustomImageList.InsertImage(Index: Integer; Image, Mask: TBitmap;
  MaskColor: TColor);
var
  I: Integer;
  LRes: Integer;
begin
  BeginUpdate;
  try
    if Index > Count then
      raise EInvalidOperation.Create(SImageIndexError);

    if MaskColor <> -1 then
      LRes := AddMasked(Image, MaskColor)
    else
      LRes := Add(Image, Mask);

    if LRes = - 1 then
      raise EInvalidOperation.Create(SInsertImage);

    for I := Count - 2 downto Index do
      ImageList_Copy(Handle, I, Handle, I + 1, ILCF_SWAP);
  finally
    EndUpdate;
  end;
end;

procedure TCustomImageList.Insert(Index: Integer; Image, Mask: TBitmap);
begin
  InsertImage(Index, Image, Mask, -1);
end;

procedure TCustomImageList.InsertMasked(Index: Integer; Image: TBitmap;
  MaskColor: TColor);
begin
  InsertImage(Index, Image, nil, MaskColor);
end;

procedure TCustomImageList.InsertIcon(Index: Integer; Image: TIcon);
var
  I: Integer;
  TempList: TCustomImageList;
  Icon: TIcon;
begin
  Icon := nil;
  TempList := nil;
  BeginUpdate;
  try
    TempList := TCustomImageList.CreateSize(5, 5);
    TempList.Assign(Self);
    Clear;
    if Index > TempList.Count then raise EInvalidOperation.Create(SImageIndexError);
    Icon := TIcon.Create;
    for I := 0 to Index - 1 do
    begin
      TempList.GetIcon(I, Icon);
      AddIcon(Icon);
    end;
    AddIcon(Image);
    for I := Index to TempList.Count - 1 do
    begin
      TempList.GetIcon(I, Icon);
      AddIcon(Icon);
    end;
  finally
    EndUpdate;
    Icon.Free;
    TempList.Free;
  end;
end;

procedure TCustomImageList.Move(CurIndex, NewIndex: Integer);
var
  I: Integer;
begin
  if CurIndex > NewIndex then
    for i := NewIndex to CurIndex - 1 do
      ImageList_Copy(Handle, I, Handle, CurIndex, ILCF_SWAP)
  else if CurIndex < NewIndex then
    for i := NewIndex downto CurIndex + 1 do
      ImageList_Copy(Handle, I, Handle, CurIndex, ILCF_SWAP);
  Change;
end;

function TCustomImageList.AddImage(Value: TCustomImageList; Index: Integer): Integer;
begin
  if Value <> nil then
  begin
    CopyImages(Value.Handle, Index);
    Result := Count;
  end else
    Result := -1;
end;

function TCustomImageList.AddDisabledImage(Value: TCustomImageList; Index: Integer): Integer;
begin
  if Value <> nil then
  begin
    CopyFromImageList(Value, Index, True);
    Result := Count;
  end else
    Result := -1;
end;

procedure TCustomImageList.AddImages(Value: TCustomImageList);
begin
  if Value <> nil then CopyImages(Value.Handle);
end;

procedure TCustomImageList.AddDisabledImages(Value: TCustomImageList);
begin
  if Value <> nil then CopyFromImageList(Value, -1, True);
end;

procedure TCustomImageList.Assign(Source: TPersistent);
var
  ImageList: TCustomImageList;
  MemStream: TMemoryStream;
begin
  if Source = nil then FreeHandle
  else if Source is TCustomImageList then
  begin
    ImageList := TCustomImageList(Source);

    MemStream := TMemoryStream.Create;
    try
      ImageList.WriteData(MemStream);
      MemStream.Position := 0;
      ReadData(MemStream);
    finally
      MemStream.Free;
    end;
  end
  else inherited Assign(Source);
end;

procedure TCustomImageList.AssignTo(Dest: TPersistent);
var
  ImageList: TCustomImageList;
  MemStream: TMemoryStream;
begin
  if Dest is TCustomImageList then
  begin
    ImageList := TCustomImageList(Dest);

    MemStream := TMemoryStream.Create;
    try
      WriteData(MemStream);
      MemStream.Position := 0;
      ImageList.ReadData(MemStream);
    finally
      MemStream.Free;
    end;
  end
  else inherited AssignTo(Dest);
end;

procedure TCustomImageList.SetDrawingStyle(Value: TDrawingStyle);
begin
  if Value <> DrawingStyle then
  begin
    FDrawingStyle := Value;
    Change;
  end;
end;

procedure TCustomImageList.SetDisabledStyle(Value: TDisabledStyle);
begin
  if Value <> DisabledStyle then
  begin
    FDisabledStyle := Value;
    Change;
  end;
end;

function TCustomImageList.GetHotSpot: TPoint;
begin
  Result := Point(0, 0);
end;

function TCustomImageList.GetInstRes(Instance: THandle; ResType: TResType;
  ResID: DWORD; Width: Integer; LoadFlags: TLoadResources;
  MaskColor: TColor): Boolean;
begin
{$IF DEFINED(CLR)}
  Result := InternalGetInstRes(Instance, ResType, '', ResID, Width,
    LoadFlags, MaskColor);
{$ELSE}
  Result := InternalGetInstRes(Instance, ResType, PChar(ResID), Width,
    LoadFlags, MaskColor);
{$ENDIF}
end;

function TCustomImageList.GetInstRes(Instance: THandle; ResType: TResType;
  const Name: string; Width: Integer; LoadFlags: TLoadResources;
  MaskColor: TColor): Boolean;
begin
{$IF DEFINED(CLR)}
  Result := InternalGetInstRes(Instance, ResType, Name, 0, Width,
    LoadFlags, MaskColor);
{$ELSE}
  Result := InternalGetInstRes(Instance, ResType, PChar(Name), Width,
    LoadFlags, MaskColor);
{$ENDIF}
end;

{$IF DEFINED(CLR)}
function TCustomImageList.InternalGetInstRes(Instance: THandle;
  ResType: TResType; Name: string; ResID: DWORD; Width: Integer;
  LoadFlags: TLoadResources; MaskColor: TColor): Boolean;
{$ELSE}
function TCustomImageList.InternalGetInstRes(Instance: THandle;
  ResType: TResType; Name: PChar; Width: Integer; LoadFlags: TLoadResources;
  MaskColor: TColor): Boolean;
{$ENDIF}
const
  ResMap: array [TResType] of Integer = (IMAGE_BITMAP, IMAGE_CURSOR, IMAGE_ICON);
var
  hImage: HImageList;
  Flags: Integer;
begin
  Flags := 0;
  if lrDefaultColor in LoadFlags then Flags := Flags or LR_DEFAULTCOLOR;
  if lrDefaultSize in LoadFlags then Flags := Flags or LR_DEFAULTSIZE;
  if lrFromFile in LoadFlags then Flags := Flags or LR_LOADFROMFILE;
  if lrMap3DColors in LoadFlags then Flags := Flags or LR_LOADMAP3DCOLORS;
  if lrTransparent in LoadFlags then Flags := Flags or LR_LOADTRANSPARENT;
  if lrMonoChrome in LoadFlags then Flags := Flags or LR_MONOCHROME;
{$IF DEFINED(CLR)}
  if Name <> '' then
    hImage := ImageList_LoadImage(Instance, Name, Width, AllocBy, MaskColor,
      ResMap[ResType], Flags)
  else
    hImage := ImageList_LoadImage(Instance, ResID, Width, AllocBy, MaskColor,
      ResMap[ResType], Flags);
{$ELSE}
  hImage := ImageList_LoadImage(Instance, Name, Width, AllocBy, MaskColor,
    ResMap[ResType], Flags);
{$ENDIF}
  if hImage <> 0 then
  begin
    CopyImages(hImage);
    ImageList_Destroy(hImage);
    Result := True;
  end
  else Result := False;
end;

function TCustomImageList.GetResource(ResType: TResType; const Name: string;
  Width: Integer; LoadFlags: TLoadResources; MaskColor: TColor): Boolean;
begin
  Result := GetInstRes(MainInstance, ResType, Name, Width, LoadFlags, MaskColor);
end;

function TCustomImageList.ResInstLoad(Instance: THandle; ResType: TResType;
  const Name: string; MaskColor: TColor): Boolean;
begin
  Result := GetInstRes(Instance, ResType, Name, Width, [], MaskColor);
end;

/// ResourceLoad looks in currently loaded assemblies but does not
/// explicitly load resource assemblies based on locale override
function TCustomImageList.ResourceLoad(ResType: TResType; const Name: string;
  MaskColor: TColor): Boolean;
{$IF DEFINED(CLR)}
var
  I: Integer;
  ResInstance: THandle;
  LAssemblies: array of System.Reflection.Assembly;
begin
  Result := False;
  if HInstance = MainInstance then
    Result := GetInstRes(MainInstance, ResType, Name, Width, [], MaskColor)
  else
  begin
    LAssemblies := AppDomain.CurrentDomain.GetAssemblies;
    for I := 0 to Length(LAssemblies) - 1 do
    begin
      ResInstance := THandle(Marshal.GetHInstance(LAssemblies[I].GetModules[0]));
      Result := GetInstRes(ResInstance, ResType, Name, Width, [], MaskColor);
      if Result then Exit;
    end;
  end;
end;
{$ELSE}
var
  LibModule: PLibModule;
begin
  Result := False;
  if HInstance = MainInstance then
    Result := GetInstRes(MainInstance, ResType, Name, Width, [], MaskColor)
  else
  begin
    LibModule := LibModuleList;
    while LibModule <> nil do
      with LibModule^ do
      begin
        Result := GetInstRes(ResInstance, ResType, Name, Width, [], MaskColor);
        if not Result and (Instance <> ResInstance) then
          Result := GetInstRes(Instance, ResType, Name, Width, [], MaskColor);
        if Result then Exit;
        LibModule := LibModule.Next;
      end;
  end;
end;
{$ENDIF}

{$IFDEF CLR}[FileIOPermission(SecurityAction.LinkDemand, Unrestricted=True)]{$ENDIF}
function TCustomImageList.FileLoad(ResType: TResType; const Name: string;
  MaskColor: TColor): Boolean;
begin
  Result := GetResource(ResType, Name, Width, [lrFromFile], MaskColor);
end;

procedure TCustomImageList.DoChange;
var
  I: Integer;
begin
  for I := 0 to LinkCount - 1 do
    Links[I].Change;
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TCustomImageList.IsScaled: Boolean;
begin
  Result := FScaled;
end;

procedure TCustomImageList.UnRegisterChanges(Value: TChangeLink);
begin
  DeleteLink(Value);
end;

procedure TCustomImageList.RegisterChanges(Value: TChangeLink);
begin
  AddLink(Value);
end;

function TCustomImageList.Equal(IL: TCustomImageList): Boolean;

  function StreamsEqual(S1, S2: TMemoryStream): Boolean;
  begin
{$IF DEFINED(CLR)}
    Result := (S1.Size = S2.Size) and System.Array(S1.Memory).Equals(TObject(S2));
{$ELSE}
    Result := (S1.Size = S2.Size) and CompareMem(S1.Memory, S2.Memory, S1.Size);
{$ENDIF}
  end;

var
  MyImage, OtherImage: TMemoryStream;
begin
  if (IL = nil) or (Count <> IL.Count) then
  begin
    Result := False;
    Exit;
  end;
  if (Count = 0) and (IL.Count = 0) then
  begin
    Result := True;
    Exit;
  end;
  MyImage := TMemoryStream.Create;
  try
    WriteData(MyImage);
    OtherImage := TMemoryStream.Create;
    try
      IL.WriteData(OtherImage);
      Result := StreamsEqual(MyImage, OtherImage);
    finally
      OtherImage.Free;
    end;
  finally
    MyImage.Free;
  end;
end;

procedure TCustomImageList.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if not FStoreBitmap then
      Result := False
    else
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TCustomImageList) or
        not Equal(TCustomImageList(Filer.Ancestor))
    else
      Result := Count > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Bitmap', ReadData, WriteData, DoWrite);
end;

procedure TCustomImageList.ReadD2Stream(Stream: TStream);
var
  FullImage, Image, FullMask, Mask: TBitmap;
  I, J, Size, Pos, Count: Integer;
  SrcRect: TRect;
begin
  Stream.ReadBuffer(Size, SizeOf(Size));
  Stream.ReadBuffer(Count, SizeOf(Count));
  FullImage := TBitmap.Create;
  try
    Pos := Stream.Position;
    FullImage.LoadFromStream(Stream);
    Stream.Position := Pos + Size;
    FullMask := TBitmap.Create;
    try
      FullMask.LoadFromStream(Stream);
      Image := TBitmap.Create;
      Image.Width := Width;
      Image.Height := Height;
      Mask := TBitmap.Create;
      Mask.Monochrome := True;
      Mask.Width := Width;
      Mask.Height := Height;
      SrcRect := Rect(0, 0, Width, Height);
      BeginUpdate;
      try
        for J := 0 to (FullImage.Height div Height) - 1 do
        begin
          if Count = 0 then Break;
          for I := 0 to (FullImage.Width div Width) - 1 do
          begin
            if Count = 0 then Break;
            Image.Canvas.CopyRect(SrcRect, FullImage.Canvas,
              Bounds(I * Width, J * Height, Width, Height));
            Mask.Canvas.CopyRect(SrcRect, FullMask.Canvas,
              Bounds(I * Width, J * Height, Width, Height));
            Add(Image, Mask);
            Dec(Count);
          end;
        end;
      finally
        Image.Free;
        Mask.Free;
        EndUpdate;
      end;
    finally
      FullMask.Free;
    end;
  finally
    FullImage.Free;
  end;
end;

procedure TCustomImageList.ReadD3Stream(Stream: TStream);
var
  LAdapter: TStreamAdapter;
  LTemp: TMemoryStream;
  LRetry: Boolean;
  LValue, LBitCount: Byte;
begin
  // attempt a simple read
  LAdapter := TStreamAdapter.Create(Stream);
  try
    Handle := ImageList_Read(LAdapter);
  finally
    LAdapter.Free;
  end;

  // if we were not successful then attempt to fix up the really old ComCtl stream
  if not HandleAllocated then
  begin

    // make a temp copy of the stream
    LRetry := False;
    LTemp := TMemoryStream.Create;
    try
      Stream.Position := 0;
      LTemp.LoadFromStream(Stream);

      // find the bad value imagelist header info
      LTemp.Position := 18;
      if (LTemp.Read(LValue, 1) = 1) and (LValue = 1) then
      begin

        // find the bitcount data farther on into the BitmapInfoHeader
        LTemp.Position := 56;
        if LTemp.Read(LBitCount, 1) = 1 then
        begin

          // correct the original value
          LValue := LValue or LBitCount;

          // back to the imagelist header info and patch it
          LTemp.Position := 18;
{$IF DEFINED(CLR)}
          LRetry := LTemp.Write(Integer(LValue), 1) = 1;
{$ELSE}
          LRetry := LTemp.Write(LValue, 1) = 1;
{$ENDIF}
        end;
      end;

      // reattempt the load
      if LRetry then
      begin
        LTemp.Position := 0;
        LAdapter := TStreamAdapter.Create(LTemp);
        try
          Handle := ImageList_Read(LAdapter);
        finally
          LAdapter.Free;
        end;
      end;

    finally
      LTemp.Free;
    end;

    // if we still didn't succeed then fail
    if not HandleAllocated then
      raise EReadError.CreateRes({$IFNDEF CLR}@{$ENDIF}SImageReadFail);
  end;
end;

procedure TCustomImageList.ReadData(Stream: TStream);
var
  CheckInt1, CheckInt2: Integer;
  CheckByte1, CheckByte2: Byte;
  StreamPos: Integer;
begin
  FreeHandle;
  StreamPos := Stream.Position;              // check stream signature to
  Stream.Read(CheckInt1, SizeOf(CheckInt1)); // determine a Delphi 2 or Delphi
  Stream.Read(CheckInt2, SizeOf(CheckInt2)); // 3 imagelist stream.  Delphi 2
  CheckByte1 := LoByte(LoWord(CheckInt1));   // streams can be read, but only
  CheckByte2 := HiByte(LoWord(CheckInt1));   // Delphi 3 streams will be written
  Stream.Position := StreamPos;
  if (CheckInt1 <> CheckInt2) and (CheckByte1 = $49) and (CheckByte2 = $4C) then
    ReadD3Stream(Stream)
  else
    ReadD2Stream(Stream);
  if not StyleServices.Enabled then
    ImageList_SetImageCount(Handle, ImageList_GetImageCount(Handle));
end;

type
  {$IFDEF CLR}[SuppressUnmanagedCodeSecurity]{$ENDIF}
  TImageListWriteExProc = function(ImageList: HIMAGELIST; Flags: DWORD;
    Stream: IStream): HRESULT; {$IFNDEF CLR}stdcall;{$ENDIF}

const
  ComCtlVersionIE6 = $00060000;

var
  CachedComCtrlVer: Cardinal;
  ImageListWriteExProc: TImageListWriteExProc;

procedure TCustomImageList.WriteData(Stream: TStream);
var
  SA: TStreamAdapter;
  ComCtrlHandle: THandle;
  SavePos: Int64;
  NewPos: Int64;
  WordValue: UInt16;
const
  ILP_DOWNLEVEL = 1;
begin
  if CachedComCtrlVer = 0 then
  begin
    CachedComCtrlVer := GetFileVersion(comctl32);
    if CachedComCtrlVer >= ComCtlVersionIE6 then
    begin
      ComCtrlHandle := GetModuleHandle(comctl32);
      if ComCtrlHandle <> 0 then
{$IF DEFINED(CLR)}
        BindProcAddress(ImageListWriteExProc, TypeOf(TImageListWriteExProc),
          ComCtrlHandle, 'ImageList_WriteEx'); { Do not localize }
{$ELSE}
        ImageListWriteExProc := GetProcAddress(ComCtrlHandle, 'ImageList_WriteEx'); { Do not localize }
{$ENDIF}
    end;
  end;

  SA := TStreamAdapter.Create(Stream);
  try
    SavePos := Stream.Position;

    { See if we should use the new API for writing image lists in the old format. }
    if Assigned(ImageListWriteExProc) then
    begin
      if ImageListWriteExProc(Handle, ILP_DOWNLEVEL, SA) <> S_OK then
        raise EWriteError.CreateRes({$IFNDEF CLR}@{$ENDIF}SImageWriteFail);
    end
    else if not ImageList_Write(Handle, SA) then
        raise EWriteError.CreateRes({$IFNDEF CLR}@{$ENDIF}SImageWriteFail);

    NewPos := Stream.Position;
    Stream.Position := SavePos;
    Stream.Read(WordValue, SizeOf(WordValue));
    { Checking for 'IL', the magic signature of ImageList per 'https://source.winehq.org/source/dlls/comctl32/imagelist.c' }
    if (WordRec(WordValue).Lo = $49) and (WordRec(WordValue).Hi = $4C) then
    begin
      { Setting cGrow value to AllocBy in image header }
      Stream.Position := SavePos + 8;
      WordValue := AllocBy;
      Stream.Write(WordValue, SizeOf(WordValue));
    end;

    Stream.Position := NewPos;
  finally
    SA.Free;
  end;
end;

function TCustomImageList.IsImageNameAvailable: Boolean;
begin
  Result := False;
end;

function TCustomImageList.GetIndexByName(const AName: TImageName): System.UITypes.TImageIndex;
begin
  Result := -1;
end;

function TCustomImageList.GetNameByIndex(AIndex: System.UITypes.TImageIndex): TImageName;
begin
  Result := '';
end;

procedure TCustomImageList.CheckIndexAndName(var AIndex: System.UITypes.TImageIndex; var AName: TImageName);
var
  LName: String;
begin
  if not IsImageNameAvailable then
    Exit;

  if AName <> '' then
  begin
    if (AIndex >= 0) and (AIndex < Count) then
    begin
      LName := GetNameByIndex(AIndex);
      if not SameText(LName, AName) then
        AIndex := GetIndexByName(AName);
    end
    else
      AIndex := GetIndexByName(AName);
  end
  else
  if (AIndex >= 0) and (AIndex < Count) then
    AName := GetNameByIndex(AIndex);
end;

                           
(*
var
  I: Integer;
  DIB1, DIB2: TBitmap;
  DC: HDC;
  S: TMemoryStream;

  procedure WriteDIB(BM: HBitmap);
    { The ImageList leaves its bitmap handle selected into a DC somewhere,
      so we can't select it into our own DC to copy from it.  The only safe
      operation is GetDIB (GetDIBits), which extracts the pixel bits without
      selecting the BM into a DC.  This code builds our own bitmap from
      those bits, then crops it to the minimum size before writing it out.}
  var
    BitsSize: DWORD;
    Header, Bits: PChar;
    DIBBits: Pointer;
    R: TRect;
    HeaderSize: DWORD;
    GlyphsPerRow, Rows: Integer;
  begin
    if BM = 0 then Exit;
    GetDIBSizes(BM, HeaderSize, BitsSize);
    GetMem(Header, HeaderSize + BitsSize);
    try
      Bits := Header + HeaderSize;
      GetDIB(BM, 0, Header^, Bits^);
      DIB1.Handle := CreateDIBSection(DC, PBitmapInfo(Header)^, DIB_RGB_COLORS, DIBBits, 0, 0);
      System.Move(Bits^, DIBBits^, BitsSize);
      with PBitmapInfo(Header)^.bmiHeader do
      begin
        GlyphsPerRow := biWidth div Width;
        if GlyphsPerRow = 0 then Inc(GlyphsPerRow);
        if GlyphsPerRow > Count then GlyphsPerRow := Count;
        biWidth := GlyphsPerRow * Width;
        Rows := Count div GlyphsPerRow;
        if Count > Rows * GlyphsPerRow then Inc(Rows);
        biHeight := Rows * Height;
        R := Rect(0, 0, biWidth, biHeight);
      end;
      DIB2.Handle := CreateDIBSection(DC, PBitmapInfo(Header)^, DIB_RGB_COLORS, DIBBits, 0, 0);
      DIB2.Canvas.CopyRect(R, DIB1.Canvas, R);
      DIB2.SaveToStream(S);
    finally
      FreeMem(Header);
    end;
  end;

begin
  DIB1 := nil;
  DIB2 := nil;
  DC := 0;
  S := TMemoryStream.Create;
  try
    DIB1 := TBitmap.Create;
    DIB2 := TBitmap.Create;
    DC := GetDC(0);
    WriteDIB(GetImageBitmap);
    I := S.Size;
    WriteDIB(GetMaskBitmap);
    Stream.WriteBuffer(I, sizeof(I));
    I := Count;
    Stream.WriteBuffer(I, sizeof(I));
    Stream.WriteBuffer(S.Memory^, S.Size);
  finally
    ReleaseDC(0, DC);
    DIB1.Free;
    DIB2.Free;
    S.Free;
  end;
end;
*)

{ TChangeLink }

constructor TChangeLink.Create;
begin
  inherited;
  IgnoreIndex := True;
  IgnoreImages := True;
end;

function TChangeLink.GetSender: TCustomImageList;
begin
  Result := TCustomImageList(Images);
end;

procedure TChangeLink.SetSender(const Value: TCustomImageList);
begin
  Images := TBaseImageList(Value);
end;

end.
