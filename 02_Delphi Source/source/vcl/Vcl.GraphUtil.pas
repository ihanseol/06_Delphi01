{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Vcl.GraphUtil;

{$HPPEMIT LEGACYHPP}

interface

uses
{$IF DEFINED(CLR) OR DEFINED(MSWINDOWS)}
  Winapi.Windows, Vcl.Graphics,
{$ENDIF}
{$IF DEFINED(LINUX)}
  Types, QGraphics, 
{$ENDIF}
  System.Classes;

{$IF NOT DEFINED(CLR)}
  {$DEFINE USE_ZLIB}
{$ENDIF}

type
  TColorArray = array of TIdentMapEntry;

const
  WebNamedColorsCount = 138;
  WebNamedColors: array[0..WebNamedColorsCount - 1] of TIdentMapEntry = (
    // light colors snow -> tan
    (Value: clWebSnow; Name: 'clWebSnow'),                                { do not localize }
    (Value: clWebFloralWhite; Name: 'clWebFloralWhite'),                  { do not localize }
    (Value: clWebLavenderBlush; Name: 'clWebLavenderBlush'),              { do not localize }
    (Value: clWebOldLace; Name: 'clWebOldLace'),                          { do not localize }
    (Value: clWebIvory; Name: 'clWebIvory'),                              { do not localize }
    (Value: clWebCornSilk; Name: 'clWebCornSilk'),                        { do not localize }
    (Value: clWebBeige; Name: 'clWebBeige'),                              { do not localize }
    (Value: clWebAntiqueWhite; Name: 'clWebAntiqueWhite'),                { do not localize }
    (Value: clWebWheat; Name: 'clWebWheat'),                              { do not localize }
    (Value: clWebAliceBlue; Name: 'clWebAliceBlue'),                      { do not localize }
    (Value: clWebGhostWhite; Name: 'clWebGhostWhite'),                    { do not localize }
    (Value: clWebLavender; Name: 'clWebLavender'),                        { do not localize }
    (Value: clWebSeashell; Name: 'clWebSeashell'),                        { do not localize }
    (Value: clWebLightYellow; Name: 'clWebLightYellow'),                  { do not localize }
    (Value: clWebPapayaWhip; Name: 'clWebPapayaWhip'),                    { do not localize }
    (Value: clWebNavajoWhite; Name: 'clWebNavajoWhite'),                  { do not localize }
    (Value: clWebMoccasin; Name: 'clWebMoccasin'),                        { do not localize }
    (Value: clWebBurlywood; Name: 'clWebBurlywood'),                      { do not localize }
    (Value: clWebAzure; Name: 'clWebAzure'),                              { do not localize }
    (Value: clWebMintcream; Name: 'clWebMintcream'),                      { do not localize }
    (Value: clWebHoneydew; Name: 'clWebHoneydew'),                        { do not localize }
    (Value: clWebLinen; Name: 'clWebLinen'),                              { do not localize }
    (Value: clWebLemonChiffon; Name: 'clWebLemonChiffon'),                { do not localize }
    (Value: clWebBlanchedAlmond; Name: 'clWebBlanchedAlmond'),            { do not localize }
    (Value: clWebBisque; Name: 'clWebBisque'),                            { do not localize }
    (Value: clWebPeachPuff; Name: 'clWebPeachPuff'),                      { do not localize }
    (Value: clWebTan; Name: 'clWebTan'),                                  { do not localize }
  // yellows/reds yellow -> rosybrown
    (Value: clWebYellow; Name: 'clWebYellow'),                            { do not localize }
    (Value: clWebDarkOrange; Name: 'clWebDarkOrange'),                    { do not localize }
    (Value: clWebRed; Name: 'clWebRed'),                                  { do not localize }
    (Value: clWebDarkRed; Name: 'clWebDarkRed'),                          { do not localize }
    (Value: clWebMaroon; Name: 'clWebMaroon'),                            { do not localize }
    (Value: clWebIndianRed; Name: 'clWebIndianRed'),                      { do not localize }
    (Value: clWebSalmon; Name: 'clWebSalmon'),                            { do not localize }
    (Value: clWebCoral; Name: 'clWebCoral'),                              { do not localize }
    (Value: clWebGold; Name: 'clWebGold'),                                { do not localize }
    (Value: clWebTomato; Name: 'clWebTomato'),                            { do not localize }
    (Value: clWebCrimson; Name: 'clWebCrimson'),                          { do not localize }
    (Value: clWebBrown; Name: 'clWebBrown'),                              { do not localize }
    (Value: clWebChocolate; Name: 'clWebChocolate'),                      { do not localize }
    (Value: clWebSandyBrown; Name: 'clWebSandyBrown'),                    { do not localize }
    (Value: clWebLightSalmon; Name: 'clWebLightSalmon'),                  { do not localize }
    (Value: clWebLightCoral; Name: 'clWebLightCoral'),                    { do not localize }
    (Value: clWebOrange; Name: 'clWebOrange'),                            { do not localize }
    (Value: clWebOrangeRed; Name: 'clWebOrangeRed'),                      { do not localize }
    (Value: clWebFirebrick; Name: 'clWebFirebrick'),                      { do not localize }
    (Value: clWebSaddleBrown; Name: 'clWebSaddleBrown'),                  { do not localize }
    (Value: clWebSienna; Name: 'clWebSienna'),                            { do not localize }
    (Value: clWebPeru; Name: 'clWebPeru'),                                { do not localize }
    (Value: clWebDarkSalmon; Name: 'clWebDarkSalmon'),                    { do not localize }
    (Value: clWebRosyBrown; Name: 'clWebRosyBrown'),                      { do not localize }
  // greens palegoldenrod -> darkseagreen
    (Value: clWebPaleGoldenrod; Name: 'clWebPaleGoldenrod'),              { do not localize }
    (Value: clWebLightGoldenrodYellow; Name: 'clWebLightGoldenrodYellow'),{ do not localize }
    (Value: clWebOlive; Name: 'clWebOlive'),                              { do not localize }
    (Value: clWebForestGreen; Name: 'clWebForestGreen'),                  { do not localize }
    (Value: clWebGreenYellow; Name: 'clWebGreenYellow'),                  { do not localize }
    (Value: clWebChartreuse; Name: 'clWebChartreuse'),                    { do not localize }
    (Value: clWebLightGreen; Name: 'clWebLightGreen'),                    { do not localize }
    (Value: clWebAquamarine; Name: 'clWebAquamarine'),                    { do not localize }
    (Value: clWebSeaGreen; Name: 'clWebSeaGreen'),                        { do not localize }
    (Value: clWebGoldenRod; Name: 'clWebGoldenRod'),                      { do not localize }
    (Value: clWebKhaki; Name: 'clWebKhaki'),                              { do not localize }
    (Value: clWebOliveDrab; Name: 'clWebOliveDrab'),                      { do not localize }
    (Value: clWebGreen; Name: 'clWebGreen'),                              { do not localize }
    (Value: clWebYellowGreen; Name: 'clWebYellowGreen'),                  { do not localize }
    (Value: clWebLawnGreen; Name: 'clWebLawnGreen'),                      { do not localize }
    (Value: clWebPaleGreen; Name: 'clWebPaleGreen'),                      { do not localize }
    (Value: clWebMediumAquamarine; Name: 'clWebMediumAquamarine'),        { do not localize }
    (Value: clWebMediumSeaGreen; Name: 'clWebMediumSeaGreen'),            { do not localize }
    (Value: clWebDarkGoldenRod; Name: 'clWebDarkGoldenRod'),              { do not localize }
    (Value: clWebDarkKhaki; Name: 'clWebDarkKhaki'),                      { do not localize }
    (Value: clWebDarkOliveGreen; Name: 'clWebDarkOliveGreen'),            { do not localize }
    (Value: clWebDarkgreen; Name: 'clWebDarkgreen'),                      { do not localize }
    (Value: clWebLimeGreen; Name: 'clWebLimeGreen'),                      { do not localize }
    (Value: clWebLime; Name: 'clWebLime'),                                { do not localize }
    (Value: clWebSpringGreen; Name: 'clWebSpringGreen'),                  { do not localize }
    (Value: clWebMediumSpringGreen; Name: 'clWebMediumSpringGreen'),      { do not localize }
    (Value: clWebDarkSeaGreen; Name: 'clWebDarkSeaGreen'),                { do not localize }
  // greens/blues lightseagreen -> navy
    (Value: clWebLightSeaGreen; Name: 'clWebLightSeaGreen'),              { do not localize }
    (Value: clWebPaleTurquoise; Name: 'clWebPaleTurquoise'),              { do not localize }
    (Value: clWebLightCyan; Name: 'clWebLightCyan'),                      { do not localize }
    (Value: clWebLightBlue; Name: 'clWebLightBlue'),                      { do not localize }
    (Value: clWebLightSkyBlue; Name: 'clWebLightSkyBlue'),                { do not localize }
    (Value: clWebCornFlowerBlue; Name: 'clWebCornFlowerBlue'),            { do not localize }
    (Value: clWebDarkBlue; Name: 'clWebDarkBlue'),                        { do not localize }
    (Value: clWebIndigo; Name: 'clWebIndigo'),                            { do not localize }
    (Value: clWebMediumTurquoise; Name: 'clWebMediumTurquoise'),          { do not localize }
    (Value: clWebTurquoise; Name: 'clWebTurquoise'),                      { do not localize }
    (Value: clWebCyan; Name: 'clWebCyan'),                                { do not localize }
//    (Value: clWebAqua; Name: 'clWebAqua'),                                { do not localize }
    (Value: clWebPowderBlue; Name: 'clWebPowderBlue'),                    { do not localize }
    (Value: clWebSkyBlue; Name: 'clWebSkyBlue'),                          { do not localize }
    (Value: clWebRoyalBlue; Name: 'clWebRoyalBlue'),                      { do not localize }
    (Value: clWebMediumBlue; Name: 'clWebMediumBlue'),                    { do not localize }
    (Value: clWebMidnightBlue; Name: 'clWebMidnightBlue'),                { do not localize }
    (Value: clWebDarkTurquoise; Name: 'clWebDarkTurquoise'),              { do not localize }
    (Value: clWebCadetBlue; Name: 'clWebCadetBlue'),                      { do not localize }
    (Value: clWebDarkCyan; Name: 'clWebDarkCyan'),                        { do not localize }
    (Value: clWebTeal; Name: 'clWebTeal'),                                { do not localize }
    (Value: clWebDeepSkyBlue; Name: 'clWebDeepskyBlue'),                  { do not localize }
    (Value: clWebDodgerBlue; Name: 'clWebDodgerBlue'),                    { do not localize }
    (Value: clWebBlue; Name: 'clWebBlue'),                                { do not localize }
    (Value: clWebNavy; Name: 'clWebNavy'),                                { do not localize }
  // violets/pinks darkviolet -> pink
    (Value: clWebDarkViolet; Name: 'clWebDarkViolet'),                    { do not localize }
    (Value: clWebDarkOrchid; Name: 'clWebDarkOrchid'),                    { do not localize }
    (Value: clWebMagenta; Name: 'clWebMagenta'),                          { do not localize }
//    (Value: clWebFuchsia; Name: 'clWebFuchsia'),                          { do not localize }
    (Value: clWebDarkMagenta; Name: 'clWebDarkMagenta'),                  { do not localize }
    (Value: clWebMediumVioletRed; Name: 'clWebMediumVioletRed'),          { do not localize }
    (Value: clWebPaleVioletRed; Name: 'clWebPaleVioletRed'),              { do not localize }
    (Value: clWebBlueViolet; Name: 'clWebBlueViolet'),                    { do not localize }
    (Value: clWebMediumOrchid; Name: 'clWebMediumOrchid'),                { do not localize }
    (Value: clWebMediumPurple; Name: 'clWebMediumPurple'),                { do not localize }
    (Value: clWebPurple; Name: 'clWebPurple'),                            { do not localize }
    (Value: clWebDeepPink; Name: 'clWebDeepPink'),                        { do not localize }
    (Value: clWebLightPink; Name: 'clWebLightPink'),                      { do not localize }
    (Value: clWebViolet; Name: 'clWebViolet'),                            { do not localize }
    (Value: clWebOrchid; Name: 'clWebOrchid'),                            { do not localize }
    (Value: clWebPlum; Name: 'clWebPlum'),                                { do not localize }
    (Value: clWebThistle; Name: 'clWebThistle'),                          { do not localize }
    (Value: clWebHotPink; Name: 'clWebHotPink'),                          { do not localize }
    (Value: clWebPink; Name: 'clWebPink'),                                { do not localize }
  // blue/gray/black lightsteelblue -> black
    (Value: clWebLightSteelBlue; Name: 'clWebLightSteelBlue'),            { do not localize }
    (Value: clWebMediumSlateBlue; Name: 'clWebMediumSlateBlue'),          { do not localize }
    (Value: clWebLightSlateGray; Name: 'clWebLightSlateGray'),            { do not localize }
    (Value: clWebWhite; Name: 'clWebWhite'),                              { do not localize }
    (Value: clWebLightgrey; Name: 'clWebLightgrey'),                      { do not localize }
    (Value: clWebGray; Name: 'clWebGray'),                                { do not localize }
    (Value: clWebSteelBlue; Name: 'clWebSteelBlue'),                      { do not localize }
    (Value: clWebSlateBlue; Name: 'clWebSlateBlue'),                      { do not localize }
    (Value: clWebSlateGray; Name: 'clWebSlateGray'),                      { do not localize }
    (Value: clWebWhiteSmoke; Name: 'clWebWhiteSmoke'),                    { do not localize }
    (Value: clWebSilver; Name: 'clWebSilver'),                            { do not localize }
    (Value: clWebDimGray; Name: 'clWebDimGray'),                          { do not localize }
    (Value: clWebMistyRose; Name: 'clWebMistyRose'),                      { do not localize }
    (Value: clWebDarkSlateBlue; Name: 'clWebDarkSlateBlue'),              { do not localize }
    (Value: clWebDarkSlategray; Name: 'clWebDarkSlategray'),              { do not localize }
    (Value: clWebGainsboro; Name: 'clWebGainsboro'),                      { do not localize }
    (Value: clWebDarkGray; Name: 'clWebDarkGray'),                        { do not localize }
    (Value: clWebBlack; Name: 'clWebBlack'));                             { do not localize }

type
  TScrollDirection = (sdLeft, sdRight, sdUp, sdDown);
  TArrowType = (atSolid, atArrows);

/// <summary>
///  Calculates a highlight color for Color, suitable for use as the bright
///  color if drawing an old-style (non-Aero) 3D button, where the button is
///  filled with Color with a bright highlight left and top edge, and dark shadow
///  bottom and right edge. In fact, this method special-cases Color = clBtnFace
///  (with default Luminance) to return clBtnHighlight. It calculates a suitable
///  color in all other cases.
///
///  Use GetShadowColor to find a suitable shadow color.
///
///  Calculation is done in the HLS color space.
///
///  If the color's saturation is beyond 220 then it returns the input color with
///  luminance is decreased rather than increased.
///
///  Since this method may be called repeatedly for (potentially)
///  the same color value it caches the results of the previous call.
/// </summary>
/// <seealso cref="GetShadowColor"/>
function GetHighLightColor(const Color: TColor; Luminance: Integer = 19): TColor;

/// <summary>
///  Calculates a shadow color for Color, suitable for use as the dark
///  color if drawing an old-style (non-Aero) 3D button, where the button is
///  filled with Color with a bright highlight left and top edge, and dark shadow
///  bottom and right edge. In fact, this method special-cases Color = clBtnFace
///  (with default Luminance) to return clBtnShadow. It calculates a suitable
///  color in all other cases.
///
///  Use GetHighLightColor to find a suitable highlight color.
///
///  Calculation is done in the HLS color space.
///
///  Since this method may be called repeatedly for (potentially)
///  the same color value it caches the results of the previous call.
/// </summary>
/// <seealso cref="GetHighLightColor"/>
function GetShadowColor(const Color: TColor; Luminance: Integer = -50): TColor;

/// <summary>
///  Draws checkmarks of any Size at Location. Pass true to Shadow to draw
///  the checkmark with a shadow: this draws a white checkmark one pixel to the
///  right and below the checkmark, ie the shadow is white.
///  The checkmark and its shadow are each always 1 pixel thick.
///
///  Note: This method is not suitable for most uses today. It draws only one
///  pixel thick regardless of high DPI settings, and its shadow is always white
///  so not suitable for many color schemes.
///  It is marked deprecated. Possible alernatives include drawing checkmarks
///  using a check character via text output.
/// </summary>
procedure DrawCheck(ACanvas: TCanvas; Location: TPoint; Size: Integer;
  Shadow: Boolean = True); deprecated;

/// <summary>
///  Draws a chevron, which is an arrow that looks like ">", pointing in any
///  TScrollDirection.
///  The IDE uses this method to draw chevrons on its treeviews.
/// </summary>
procedure DrawChevron(ACanvas: TCanvas; Direction: TScrollDirection;
  Location: TPoint; Size: Integer);

/// <summary>
///  Draws a solid triangular arrow that can point in any TScrollDirection.
///  Location specifies the top left of the rectangle containing the arrow.
/// </summary>
procedure DrawArrow(ACanvas: TCanvas; Direction: TScrollDirection;
  Location: TPoint; Size: Integer);

{ The following routines mimic the like named routines from Shlwapi.dll except
  these routines do not rely on any specific version of IE being installed. }

/// <summary>
///  Calculates the Hue, Luminance and Saturation values for the clrRGB color.
///  That is, this method converts from RGB space to HLS space.
/// </summary>
/// <seealso cref="ColorHLSToRGB"/>
procedure ColorRGBToHLS(clrRGB: TColorRef; var Hue, Luminance, Saturation: Word);

/// <summary>
///  Calculates a RGB color given Hue, Luminance and Saturation values. That is,
///  this method converts from HLS space to RGB space.
/// </summary>
/// <seealso cref="ColorRGBToHLS"/>
function ColorHLSToRGB(Hue, Luminance, Saturation: Word): TColorRef;

/// <summary>
///  Given a color and a luminance change "n", this routine returns a color the
///  luminance of which has been changed accordingly.
/// </summary>
function ColorAdjustLuma(clrRGB: TColor; n: Integer; fScale: BOOL): TColor;

type
  TGradientDirection = (gdHorizontal, gdVertical);

/// <summary>
///  GradientFillCanvas fills the ARect rectangle on ACanvas with a gradient between
///  AStartColor and AEndColor in the given Direction (a horizontal or vertical
///  gradient.)
/// </summary>
procedure GradientFillCanvas(const ACanvas: TCanvas;
  const AStartColor, AEndColor: TColor; const ARect: TRect;
  const Direction: TGradientDirection);

/// <summary>
///  ScaleImage scales SourceBitmap into ResizedBitmap by ScaleAmount. A
///  ScaleAmount of 1 does nothing, < 0 shrinks, and > 0 enlarges. If ScaleAmount
///  is 0.5, the resulting bitmap will be sized to half the height and width of
///  the input; if ScaleAmount is 2, the resulting bitmap will be sized to double
///  the height and width of the input. Sizes are rounded.
///
///  The method only supports bitmaps with a PixelFormat of pf24bit or pf32bit.
///  Calling it with any other PixelFormat will raise an EInvalidGraphic
///  exception.
///
///  The method does not use advanced interpolation, even bilinear. When shrinking,
///  it finds all pixels that, rounding up from the destination pixel, contribute
///  to the pixel and averages their colours in each RGB channel. When enlarging,
///  it finds the four nearest pixels which contribute to the destination pixel
///  and averages those, weighted by how close each pixel is, ie how  much it
///  contributes to the output pixel. This is fast, and produces reasonable results,
///  but for better quality scaling we recommend using TImageCollection.Draw.
///
///  Note: The SourceBitmap will have its PixelFormat changed to the PixelFormat
///  specified.
///
///  See also the help page: Supporting_high-DPI_images_with_the_Image_Collection_and_Virtual_ImageList_components#Smooth_scaling_when_drawing_on_a_TCanvas
/// </summary>
procedure ScaleImage(const SourceBitmap, ResizedBitmap: TBitmap;
  const ScaleAmount: Double; PixelFormat: TPixelFormat = pf24bit);

/// <summary>
///  Blends two TColors in RGB space. Mu is 0-1, 0 being fully From, 1 being fully To.
/// </summary>
function ColorBlendRGB(const From, &To : TColor; const Mu : Single) : TColor;

/// <summary>
///  Converts a TColor to a Web color constant (like #FFFFFF).
/// </summary>
function ColorToWebColorStr(Color: TColor): string;

/// <summary>
///  ColorToWebColorName converts a TColor to a Web color name, or returns a Web
///  color value if the color is not a match.
/// </summary>
function ColorToWebColorName(Color: TColor): string;
/// <summary>
///  WebColorToRGB converts a web color to a RGB color.
/// </summary>
function WebColorToRGB(WebColor: Integer): Integer;
/// <summary>
///  RGBToWebColorStr converts a RGB color (ie, expressed as RGB bytes, see the
///  RGB() function) to a string in web color format, eg #FFFFFF.
///  Use ColorToWebColorStr if you want to convert a VCL TColor.
/// </summary>
/// <seealso cref="ColorToWebColorStr"/>
function RGBToWebColorStr(RGB: Integer): string;
/// <summary>
///  RGBToWebColorName converts a RGB color (ie, expressed as RGB bytes, see the
///  RGB() function) to a the name of that color, or if there is no matching web
///  color constant, returns the color as a RGB web string (eg #FFFFFF).
/// </summary>
/// <seealso cref="Winapi.Windows.RGB"/>
function RGBToWebColorName(RGB: Integer): string;

/// <summary>
///  WebColorNameToColor converts a Web color name to its TColor equivalent,
///  or returns clNone if there is no match.
/// </summary>
function WebColorNameToColor(WebColorName: string): TColor;

/// <summary>
///  WebColorStrToColor converts a web style color string (#FFFFFF or FFFFFF) to a TColor.
/// </summary>
function WebColorStrToColor(WebColor: string): TColor;

type
  TColorArraySortType = (stHue, stSaturation, stLuminance, stRed, stGreen, stBlue, stCombo);

/// <summary>
///  SortColorArray performs a quicksort on ColorArray according to the SortType.
/// </summary>
procedure SortColorArray(ColorArray: TColorArray; L, R: Integer;
  SortType: TColorArraySortType; Reverse: Boolean = False);

/// <summary>
///  DrawTransparentBitmap draws the specified Source bitmap to the Destination
///  canvas, stretching to fit the DestRect rectangle.
///  If the Source bitmap has PixelFormat pf32bit, it uses the alpha channel to
///  blend.
///  The second overload draws only a subsection of the Source bitmap specified
///  by SourceRect.
/// </summary>
procedure DrawTransparentBitmap(Source: TBitmap; Destination: TCanvas; DestRect: TRect; Opacity: Byte); overload;
procedure DrawTransparentBitmap(Source: TBitmap; SourceRect: TRect; Destination: TCanvas; DestRect: TRect; Opacity: Byte); overload;

function SplitTransparentBitmap(Source: TBitmap; SourceRect: TRect): TBitmap;

{$IFDEF USE_ZLIB}
function LoadCompressedResourceBitmap(ResID: string): TBitmap;
{$ENDIF USE_ZLIB}

/// <summary>
///  GetRGB extracts the red, green and blue components of the color (as bytes,
///  0-255) and returns them to the variables specified for the R, G and B
///  parameters.
/// </summary>
procedure GetRGB(Col: TColor; var R, G, B: Byte);

/// <summary>
///  ColorIsBright converts the color to grayscale on a 0-1 scale, using the
///  NTSC grayscale algorithm. It returns false if the gray color is less than
///  0.5, and true otherwise.
/// </summary>
function ColorIsBright(AColor: TColor) : Boolean;

/// <summary>
/// Set Alpha value for all pixels in the Source 32-bit bitmap
/// </summary>
procedure SetPreMutipliedAlpha(ABitMap: TBitmap; Alpha: Byte = 255);

/// <summary>
/// Set Color for all pixels in the Source 32-bit bitmap
/// </summary>
procedure SetPreMutipliedColor(ABitMap: TBitmap; Color: TColor);

procedure FillRectAlpha(Canvas: TCanvas; ARect: TRect; AColor: TColor; Alpha: Byte = 255);

procedure InitAlpha(ABitmap: TBitmap; AAlpha: Byte);
function CheckAlpha(ABitmap: TBitmap): Boolean;

procedure DrawCloseIcon(ACanvas: TCanvas; ABounds: TRect; AColor: TColor; ASize: Integer; ADPI: Integer);
procedure DrawMinimizeIcon(ACanvas: TCanvas; ABounds: TRect; AColor: TColor; ASize: Integer; ADPI: Integer);
procedure DrawMaximizeIcon(ACanvas: TCanvas; ABounds: TRect; AColor: TColor; ASize: Integer; ADPI: Integer);
procedure DrawRestoreIcon(ACanvas: TCanvas; ABounds: TRect; AColor: TColor; ASize: Integer; ADPI: Integer);
procedure DrawMenuIcon(ACanvas: TCanvas; ABounds: TRect; AColor: TColor; ASize: Integer; ADPI: Integer);

implementation

uses
{$IFDEF CLR}
  System.Runtime.InteropServices, Types,
{$ENDIF}
{$IFDEF USE_ZLIB}
  System.ZLib,
{$ENDIF USE_ZLIB}
  System.Types, System.UITypes, System.SysUtils, System.Math, Vcl.Consts;

const
  ArrowPts: array[TScrollDirection, 0..2] of TPoint =
    (((X:1; Y:0), (X:0; Y:1), (X:1; Y:2)),
     ((X:0; Y:0), (X:1; Y:1), (X:0; Y:2)),
     ((X:0; Y:1), (X:1; Y:0), (X:2; Y:1)),
     ((X:0; Y:0), (X:1; Y:1), (X:2; Y:0)));

  cDefaultPixelsPerInch = 96;
  cDefaultSystemIconSize = 7;

threadvar
  CachedRGBToHLSclrRGB: TColorRef;
  CachedRGBToHLSHue: WORD;
  CachedRGBToHLSLum: WORD;
  CachedRGBToHLSSat: WORD;

{-----------------------------------------------------------------------
References:

1) J. Foley and a.van Dam, "Fundamentals of Interactive Computer Graphics",
   Addison-Wesley (IBM Systems Programming Series), Reading, MA, 664 pp., 1982.
2) MSDN online HOWTO: Converting Colors Between RGB and HLS (HBS)
   http://support.microsoft.com/support/kb/articles/Q29/2/40.ASP

  SUMMARY
  The code fragment below converts colors between RGB (Red, Green, Blue) and
  HLS/HBS (Hue, Lightness, Saturation/Hue, Brightness, Saturation).


  http://lists.w3.org/Archives/Public/www-style/1997Dec/0182.html
  http://www.math.clemson.edu/~rsimms/neat/math/hlsrgb.pas

-----------------------------------------------------------------------}

const
  HLSMAX = 240;            // H,L, and S vary over 0-HLSMAX
  RGBMAX = 255;            // R,G, and B vary over 0-RGBMAX
                           // HLSMAX BEST IF DIVISIBLE BY 6
                           // RGBMAX, HLSMAX must each fit in a byte.

  { Hue is undefined if Saturation is 0 (grey-scale)
    This value determines where the Hue scrollbar is
    initially set for achromatic colors }
  HLSUndefined = (HLSMAX*2/3);

procedure ColorRGBToHLS(clrRGB: TColorRef; var Hue, Luminance, Saturation: Word);
var
  H, L, S: Double;
  R, G, B: Word;
  cMax, cMin: Double;
  Rdelta, Gdelta, Bdelta: Word; { intermediate value: % of spread from max }
begin
  if clrRGB = CachedRGBToHLSclrRGB then
  begin
    Hue := CachedRGBToHLSHue;
    Luminance := CachedRGBToHLSLum;
    Saturation := CachedRGBToHLSSat;
    exit;
  end;
  R := GetRValue(clrRGB);
  G := GetGValue(clrRGB);
  B := GetBValue(clrRGB);

  { calculate lightness }
  cMax := System.Math.Max(System.Math.Max(R, G), B);
  cMin := System.Math.Min(System.Math.Min(R, G), B);
  L := ( ((cMax + cMin) * HLSMAX) + RGBMAX ) / ( 2 * RGBMAX);
  Luminance := Trunc(L);
  if cMax = cMin then  { r=g=b --> achromatic case }
  begin                
    Hue := Trunc(HLSUndefined);
    Saturation := 0;
  end
  else                 { chromatic case }
  begin
    { saturation }
    if Luminance <= HLSMAX/2 then
      S := ( ((cMax-cMin)*HLSMAX) + ((cMax+cMin)/2) ) / (cMax+cMin)
    else
      S := ( ((cMax-cMin)*HLSMAX) + ((2*RGBMAX-cMax-cMin)/2) ) / (2*RGBMAX-cMax-cMin);

    { hue }
    Rdelta := Trunc(( ((cMax-R)*(HLSMAX/6)) + ((cMax-cMin)/2) ) / (cMax-cMin));
    Gdelta := Trunc(( ((cMax-G)*(HLSMAX/6)) + ((cMax-cMin)/2) ) / (cMax-cMin));
    Bdelta := Trunc(( ((cMax-B)*(HLSMAX/6)) + ((cMax-cMin)/2) ) / (cMax-cMin));

    if (R = cMax) then
      H := Bdelta - Gdelta
    else if (G = cMax) then
      H := (HLSMAX/3) + Rdelta - Bdelta
    else // B == cMax
      H := ((2 * HLSMAX) / 3) + Gdelta - Rdelta;

    if (H < 0) then
      H := H + HLSMAX;
    if (H > HLSMAX) then
      H := H - HLSMAX;

    Hue := Round(H);
    Saturation := Trunc(S);
  end;
  CachedRGBToHLSclrRGB := clrRGB;
  CachedRGBToHLSHue := Hue;
  CachedRGBToHLSLum := Luminance;
  CachedRGBToHLSSat := Saturation;
end;

function HueToRGB(Lum, Sat, Hue: Double): Integer;
var
  ResultEx: Double;
begin
  { range check: note values passed add/subtract thirds of range }
  if (hue < 0) then
     hue := hue + HLSMAX;

  if (hue > HLSMAX) then
     hue := hue - HLSMAX;

  { return r,g, or b value from this tridrant }
  if (hue < (HLSMAX/6)) then
    ResultEx := Lum + (((Sat-Lum)*hue+(HLSMAX/12))/(HLSMAX/6))
  else if (hue < (HLSMAX/2)) then
    ResultEx := Sat
  else if (hue < ((HLSMAX*2)/3)) then
    ResultEx := Lum + (((Sat-Lum)*(((HLSMAX*2)/3)-hue)+(HLSMAX/12))/(HLSMAX/6))
  else
    ResultEx := Lum;
  Result := Round(ResultEx);
end;

function ColorHLSToRGB(Hue, Luminance, Saturation: Word): TColorRef;

  function RoundColor(Value: Double): Integer;
  begin
    if Value > 255 then
      Result := 255
    else
      Result := Round(Value);
  end;

var
  R,G,B: Double;               { RGB component values }
  Magic1,Magic2: Double;       { calculated magic numbers (really!) }
begin
  if (Saturation = 0) then
  begin            { achromatic case }
     R := (Luminance * RGBMAX)/HLSMAX;
     G := R;
     B := R;
     if (Hue <> HLSUndefined) then
       ;{ ERROR }
  end
  else
  begin            { chromatic case }
     { set up magic numbers }
     if (Luminance <= (HLSMAX/2)) then
        Magic2 := (Luminance * (HLSMAX + Saturation) + (HLSMAX/2)) / HLSMAX
     else
        Magic2 := Luminance + Saturation - ((Luminance * Saturation) + (HLSMAX/2)) / HLSMAX;
     Magic1 := 2 * Luminance - Magic2;

     { get RGB, change units from HLSMAX to RGBMAX }
     R := (HueToRGB(Magic1,Magic2,Hue+(HLSMAX/3))*RGBMAX + (HLSMAX/2))/HLSMAX;
     G := (HueToRGB(Magic1,Magic2,Hue)*RGBMAX + (HLSMAX/2)) / HLSMAX;
     B := (HueToRGB(Magic1,Magic2,Hue-(HLSMAX/3))*RGBMAX + (HLSMAX/2))/HLSMAX;
  end;
  Result := RGB(RoundColor(R), RoundColor(G), RoundColor(B));
end;

threadvar
  CachedHighlightLum: Integer;
  CachedHighlightColor,
  CachedHighlight: TColor;
  CachedShadowLum: Integer;
  CachedShadowColor,
  CachedShadow: TColor;
  CachedColorValue: Integer;
  CachedLumValue: Integer;
  CachedColorAdjustLuma: TColor;

function ColorAdjustLuma(clrRGB: TColor; n: Integer; fScale: BOOL): TColor;
var
  H, L, S: Word;
begin
  if (clrRGB = CachedColorValue) and (n = CachedLumValue) then
    Result := CachedColorAdjustLuma
  else
  begin
    ColorRGBToHLS(ColorToRGB(clrRGB), H, L, S);
    Result := TColor(Integer(ColorHLSToRGB(H, Word(L + n), S)));
    CachedColorValue := clrRGB;
    CachedLumValue := n;
    CachedColorAdjustLuma := Result;
  end;
end;

procedure GetRGB(Col: TColor; var R, G, B: Byte);
var
  Color: $0..$FFFFFFFF;
begin
  Color := ColorToRGB(Col);
  R := ($000000FF and Color);
  G := ($0000FF00 and Color) shr 8;
  B := ($00FF0000 and Color) shr 16;
end;

function ColorIsBright(AColor: TColor) : Boolean;
var
 R, G, B : byte;
 Delta : Double;
begin
  GetRGB(AColor, R, G, B);
  Delta := 1 - ((0.299 * R) + (0.587 * G) + (0.114 * B)) / 255;
  Result:= (Delta < 0.5);
end;

procedure SetPreMutipliedAlpha(ABitMap: TBitmap; Alpha: Byte = 255);
var
  X, Y: Integer;
  LRGBQuad: PRGBQuad;
begin
  if ABitMap.PixelFormat <> pf32bit then
    Exit;

  ABitmap.AlphaFormat := afIgnored;
  for Y := 0 to ABitMap.Height - 1 do
  begin
    LRGBQuad := ABitMap.ScanLine[Y];
    for X := 0 to ABitMap.Width - 1 do
    begin
      LRGBQuad.rgbReserved := Alpha;
      Inc(LRGBQuad);
    end;
  end;
  ABitmap.AlphaFormat := afPremultiplied;
end;

procedure SetPreMutipliedColor(ABitMap: TBitmap; Color: TColor);
var
  X, Y: Integer;
  R, G, B: Byte;
  LRGBQuad: PRGBQuad;
begin
  if ABitMap.PixelFormat <> pf32bit then
    Exit;

  Color := ColorToRGB(Color);
  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);
  ABitmap.AlphaFormat := afIgnored;
  for Y := 0 to ABitMap.Height - 1 do
  begin
    LRGBQuad := ABitMap.ScanLine[Y];
    for X := 0 to ABitMap.Width - 1 do
    begin
      LRGBQuad.rgbRed := R;
      LRGBQuad.rgbGreen := G;
      LRGBQuad.rgbBlue := B;
      Inc(LRGBQuad);
    end;
  end;
  ABitmap.AlphaFormat := afPremultiplied;
end;

type
  PRGBAArray = ^TRGBAArray;
  TRGBAArray = array[0..0] of TRGBQuad;

procedure InitAlpha(ABitmap: TBitmap; AAlpha: Byte);
var
  I: Integer;
  LLastLine: PRGBAArray;
begin
  LLastLine := ABitmap.ScanLine[ABitmap.Height - 1];
  {$IFOPT R+} {$DEFINE RANGECHECKS_ON} {$R-} {$ENDIF}
  for I := 0 to ABitmap.Width * ABitmap.Height - 1 do
    LLastLine[I].rgbReserved := AAlpha;
  {$IFDEF RANGECHECKS_ON} {$R+} {$UNDEF RANGECHECKS_ON} {$ENDIF}
end;

function CheckAlpha(ABitmap: TBitmap): Boolean;
var
  I: Integer;
  LLastLine: PRGBAArray;
begin
  Result := False;
  LLastLine := ABitmap.Scanline[ABitmap.Height - 1];
  {$IFOPT R+} {$DEFINE RANGECHECKS_ON} {$R-} {$ENDIF}
  for I := 0 to ABitmap.Width * ABitmap.Height - 1 do
    if LLastLine[I].rgbReserved > 0 then
      Exit(True);
  {$IFDEF RANGECHECKS_ON} {$R+} {$UNDEF RANGECHECKS_ON} {$ENDIF}
end;

procedure FillRectAlpha(Canvas: TCanvas; ARect: TRect; AColor: TColor; Alpha: Byte = 255);
var
  LBitmap: Tbitmap;
  LBlendFunc: TBlendFunction;
begin
  if (Alpha = 0) or (ARect.Width <= 0) or (ARect.Height <= 0) then Exit;
  LBitmap := TBitmap.Create;
  try
    LBitmap.PixelFormat := pf32bit;
    LBitmap.SetSize(ARect.Width, ARect.Height);
    LBitmap.Canvas.Brush.Color := AColor;
    LBitmap.Canvas.FillRect(Rect(0, 0, ARect.Width, ARect.Height));
    SetPreMutipliedAlpha(LBitmap, Alpha);
    LBlendFunc.BlendOp := AC_SRC_OVER;
    LBlendFunc.BlendFlags := 0;
    LBlendFunc.SourceConstantAlpha := 255;
    LBlendFunc.AlphaFormat := AC_SRC_ALPHA;
    Winapi.Windows.AlphaBlend(Canvas.Handle, ARect.Left, ARect.Top, Arect.Width, Arect.Height,
      LBitmap.Canvas.Handle, 0, 0, LBitmap.Width, LBitmap.Height, LBlendFunc);
  finally
    LBitmap.Free;
  end;
end;

function GetHighLightColor(const Color: TColor; Luminance: Integer): TColor;
var
  H, L, S: Word;
  Clr: Cardinal;
begin
  if (Color = CachedHighlightColor) and (Luminance = CachedHighlightLum) then
    Result := CachedHighlight
  else
  begin
    // Case for default luminance
    if (Color = clBtnFace) and (Luminance = 19) then
      Result := clBtnHighlight
    else
    begin
      Clr := ColorToRGB(Color);
      ColorRGBToHLS(Clr, H, L, S);
      if (S > 220) and ((L - Luminance) >= 0) and ((L - Luminance) <= High(Word)) then
        Result := ColorHLSToRGB(H, L - Luminance, S)
      else
        Result := TColor(ColorAdjustLuma(Clr, Luminance, False));
      CachedHighlightLum := Luminance;
      CachedHighlightColor := Color;
      CachedHighlight := Result;
    end;
  end;
end;

function GetShadowColor(const Color: TColor; Luminance: Integer): TColor;
var
  H, L, S: Word;
  Clr: Cardinal;
begin
  if (Color = CachedShadowColor) and (Luminance = CachedShadowLum) then
    Result := CachedShadow
  else
  begin
    // Case for default luminance
    if (Color = clBtnFace) and (Luminance = -50) then
      Result := clBtnShadow
    else
    begin
      Clr := ColorToRGB(Color);
      ColorRGBToHLS(Clr, H, L, S);
      if (S >= 160) and ((L + Luminance) >= 0) and ((L + Luminance) <= High(Word))then
        Result := ColorHLSToRGB(H, L + Luminance, S)
      else
        Result := TColor(ColorAdjustLuma(Clr, Luminance, False));
    end;
    CachedShadowLum := Luminance;
    CachedShadowColor := Color;
    CachedShadow := Result;
  end;
end;

procedure DrawCloseIcon(ACanvas: TCanvas; ABounds: TRect; AColor: TColor; ASize: Integer; ADPI: Integer);
var
  X, Y, StartX: Integer;
  LScaleFactor: Double;

  procedure DrawLines;
  var
    I: Integer;
  begin
    for I := 1 to Trunc(LScaleFactor) do
    begin
      ACanvas.MoveTo(X, Y);
      ACanvas.LineTo(X + ASize + 1 , Y + ASize + 1);
      ACanvas.MoveTo(X + ASize, Y);
      ACanvas.LineTo(X - 1, Y + ASize + 1);
      Inc(X);
    end;
  end;

begin
  ADPI := Max(cDefaultPixelsPerInch, ADPI);
  ASize := Max(cDefaultSystemIconSize, MulDiv(ASize, ADPI, cDefaultPixelsPerInch));
  LScaleFactor := Max(1, ADPI / cDefaultPixelsPerInch);
  StartX := ABounds.Left + (ABounds.Width - ASize) div 2 - Trunc(LScaleFactor * 3) div 2 + 1;
  Y := ABounds.Top + (ABounds.Height - ASize) div 2;
  if ACanvas.Brush.Style = bsSolid then
  begin
    ACanvas.Pen.Color := ColorBlendRGB(ACanvas.Brush.Color, AColor, 0.2);
    X := StartX - 1;
    DrawLines;
    X := StartX + 1;
    DrawLines;
  end;
  ACanvas.Pen.Color := AColor;
  X := StartX;
  DrawLines;
end;

procedure DrawMinimizeIcon(ACanvas: TCanvas; ABounds: TRect; AColor: TColor; ASize: Integer; ADPI: Integer);
var
  X, Y, I, H: Integer;
begin
  ADPI := Max(cDefaultPixelsPerInch, ADPI);
  ASize := Max(cDefaultSystemIconSize, MulDiv(ASize, ADPI, cDefaultPixelsPerInch));
  H := Trunc(Max(1, ADPI / cDefaultPixelsPerInch));
  X := ABounds.CenterPoint.X - ASize div 2;
  Y := ABounds.CenterPoint.Y - H div 2;
  ACanvas.Pen.Color := AColor;
  for I := 1 to H do
  begin
    ACanvas.MoveTo(X, Y);
    ACanvas.LineTo(X + ASize, Y);
    Inc(Y);
  end;
end;

procedure DrawMenuIcon(ACanvas: TCanvas; ABounds: TRect; AColor: TColor; ASize: Integer; ADPI: Integer);
var
  X, Y, H: Integer;

procedure DrawLine;
var
  I: Integer;
begin
  for I := 1 to H do
  begin
    ACanvas.MoveTo(X, Y);
    ACanvas.LineTo(X + ASize, Y);
    Inc(Y);
  end;
end;

begin
  ADPI := Max(cDefaultPixelsPerInch, ADPI);
  ASize := Max(cDefaultSystemIconSize, MulDiv(ASize, ADPI, cDefaultPixelsPerInch));
  ACanvas.Pen.Color := AColor;
  H := Trunc(Max(1, ADPI / cDefaultPixelsPerInch));
  X := ABounds.CenterPoint.X - ASize div 2;
  Y := ABounds.CenterPoint.Y - H div 2;
  DrawLine;
  Y := ABounds.CenterPoint.Y - ASize div 2;
  DrawLine;
  Y := ABounds.CenterPoint.Y + ASize div 2 - H  div 2;
  DrawLine;
end;

procedure DrawRect(ACanvas: TCanvas; ABounds: TRect; ALineWidth: Integer);
var
  I: Integer;
begin
  ACanvas.Brush.Style := bsClear;
  for I := 1 to ALineWidth do
  begin
    ACanvas.Rectangle(ABounds);
    InflateRect(ABounds, -1, -1);
  end;
end;

procedure DrawMaximizeIcon(ACanvas: TCanvas; ABounds: TRect; AColor: TColor; ASize: Integer; ADPI: Integer);
var
  X, Y, W: Integer;
begin
  ADPI := Max(cDefaultPixelsPerInch, ADPI);
  ASize := Max(cDefaultSystemIconSize, MulDiv(ASize, ADPI, cDefaultPixelsPerInch));
  W := Trunc(Max(1, ADPI / cDefaultPixelsPerInch));
  X := ABounds.CenterPoint.X - ASize div 2;
  Y := ABounds.CenterPoint.Y - ASize div 2;
  ACanvas.Pen.Color := AColor;
  DrawRect(ACanvas, TRect.Create(X, Y, X + ASize, Y + ASize), W);
end;

procedure DrawRestoreIcon(ACanvas: TCanvas; ABounds: TRect; AColor: TColor; ASize: Integer; ADPI: Integer);
var
  X, Y, W, LSpace: Integer;
  R: TRect;
  LSaveDC: Integer;
  LScaleFactor: Double;
begin
  ADPI := Max(cDefaultPixelsPerInch, ADPI);
  ASize := Max(cDefaultSystemIconSize, MulDiv(ASize, ADPI, cDefaultPixelsPerInch));
  LScaleFactor := Max(1, ADPI / cDefaultPixelsPerInch);
  W := Trunc(LScaleFactor);
  LSpace := W + Round(LScaleFactor) div 2;
  ASize := ASize - LSpace;
  X := ABounds.CenterPoint.X - ASize div 2;
  Y := ABounds.CenterPoint.Y - ASize div 2;
  ACanvas.Pen.Color := AColor;
  R := TRect.Create(X, Y, X + ASize, Y + ASize);
  OffsetRect(R, -LSpace , LSpace );
  DrawRect(ACanvas, R, W);
  LSaveDC := SaveDC(ACanvas.Handle);
  try
    ExcludeClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    OffsetRect(R, LSpace * 2, -LSpace * 2);
    DrawRect(ACanvas, R, W);
  finally
    RestoreDC(ACanvas.Handle, LSaveDC);
  end;
end;


{ Utility Drawing Routines }

procedure DrawArrow(ACanvas: TCanvas; Direction: TScrollDirection;
  Location: TPoint; Size: Integer);
var
  I: Integer;
  Pts: array[0..2] of TPoint;
  OldWidth: Integer;
  OldColor: TColor;
begin
  if ACanvas = nil then exit;
  OldColor := ACanvas.Brush.Color;
  ACanvas.Brush.Color := ACanvas.Pen.Color;
  for I := 0 to 2 do
    Pts[I] := Point(ArrowPts[Direction,I].x * Size + Location.X,
       ArrowPts[Direction,I].y * Size + Location.Y);
  with ACanvas do
  begin
    OldWidth := Pen.Width;
    Pen.Width := 1;
    Polygon(Pts);
    Pen.Width := OldWidth;
    Brush.Color := OldColor;
  end;
end;

procedure DrawChevron(ACanvas: TCanvas; Direction: TScrollDirection;
  Location: TPoint; Size: Integer);

  procedure DrawLine;
  var
    I: Integer;
    Pts: array[0..2] of TPoint;
  begin
    // Scale to the correct size
    for I := 0 to 2 do
      Pts[I] := Point(ArrowPts[Direction, I].X * Size + Location.X,
                      ArrowPts[Direction, I].Y * Size + Location.Y);
    case Direction of
      sdDown : Pts[2] := Point(Pts[2].X + 1, Pts[2].Y - 1);
      sdRight: Pts[2] := Point(Pts[2].X - 1, Pts[2].Y + 1);
      sdUp,
      sdLeft : Pts[2] := Point(Pts[2].X + 1, Pts[2].Y + 1);
    end;
    ACanvas.PolyLine(Pts);
  end;

var
  OldWidth: Integer;
begin
  if ACanvas = nil then exit;
  OldWidth := ACanvas.Pen.Width;
  ACanvas.Pen.Width := 1;
  case Direction of
    sdLeft, sdRight:
      begin
        Dec(Location.x, Size);
        DrawLine;
        Inc(Location.x);
        DrawLine;
        Inc(Location.x, 3);
        DrawLine;
        Inc(Location.x);
        DrawLine;
      end;
    sdUp, sdDown:
      begin
        Dec(Location.y, Size);
        DrawLine;
        Inc(Location.y);
        DrawLine;
        Inc(Location.y, 3);
        DrawLine;
        Inc(Location.y);
        DrawLine;
      end;
  end;
  ACanvas.Pen.Width := OldWidth;
end;

procedure DrawCheck(ACanvas: TCanvas; Location: TPoint; Size: Integer;
  Shadow: Boolean = True);
var
  PR: TPenRecall;
begin
  if ACanvas = nil then exit;
  PR := TPenRecall.Create(ACanvas.Pen);
  try
    ACanvas.Pen.Width := 1;
    ACanvas.PolyLine([
      Point(Location.X, Location.Y),
      Point(Location.X + Size, Location.Y + Size),
      Point(Location.X + Size * 2 + Size, Location.Y - Size),
      Point(Location.X + Size * 2 + Size, Location.Y - Size - 1),
      Point(Location.X + Size, Location.Y + Size - 1),
      Point(Location.X - 1, Location.Y - 2)]);
    if Shadow then
    begin
      ACanvas.Pen.Color := clWhite;
      ACanvas.PolyLine([
        Point(Location.X - 1, Location.Y - 1),
        Point(Location.X - 1, Location.Y),
        Point(Location.X, Location.Y + 1),
        Point(Location.X + Size, Location.Y + Size + 1),
        Point(Location.X + Size * 2 + Size + 1, Location.Y - Size),
        Point(Location.X + Size * 2 + Size + 1, Location.Y - Size - 1),
        Point(Location.X + Size * 2 + Size + 1, Location.Y - Size - 2)]);
    end;
  finally
    PR.Free;
  end;
end;

procedure GradientFillCanvas(const ACanvas: TCanvas;
  const AStartColor, AEndColor: TColor; const ARect: TRect;
  const Direction: TGradientDirection);
const
  cGradientDirections: array[TGradientDirection] of Cardinal =
    (GRADIENT_FILL_RECT_H, GRADIENT_FILL_RECT_V);
var
  StartColor, EndColor: Cardinal;
  Vertexes: array[0..1] of TTriVertex;
  GradientRect: TGradientRect;
begin
  StartColor := ColorToRGB(AStartColor);
  EndColor := ColorToRGB(AEndColor);

  Vertexes[0].x := ARect.Left;
  Vertexes[0].y := ARect.Top;
  Vertexes[0].Red := GetRValue(StartColor) shl 8;
  Vertexes[0].Blue := GetBValue(StartColor) shl 8;
  Vertexes[0].Green := GetGValue(StartColor) shl 8;
  Vertexes[0].Alpha := 0;

  Vertexes[1].x := ARect.Right;
  Vertexes[1].y := ARect.Bottom;
  Vertexes[1].Red := GetRValue(EndColor) shl 8;
  Vertexes[1].Blue := GetBValue(EndColor) shl 8;
  Vertexes[1].Green := GetGValue(EndColor) shl 8;
  Vertexes[1].Alpha := 0;

  GradientRect.UpperLeft := 0;
  GradientRect.LowerRight := 1;

{$IF DEFINED(CLR)}
  GradientFill(ACanvas.Handle, Vertexes, 2, GradientRect, 1,
    cGradientDirections[Direction]);
{$ELSE}
  GradientFill(ACanvas.Handle, @Vertexes[0], 2, @GradientRect, 1,
    cGradientDirections[Direction]);
{$ENDIF}
end;

procedure ShrinkImage(const SourceBitmap, StretchedBitmap: TBitmap;
  Scale: Double; PixelFormat: TPixelFormat = pf24bit);
var
{$IF DEFINED(CLR)}
  ScanLines: array of IntPtr;
  DestLine: IntPtr;
  CurrentLine: IntPtr;
{$ELSE}
  ScanLines: array of PByteArray;
  DestLine: PByteArray;
  CurrentLine: PByteArray;
{$ENDIF}
  DestX, DestY: Integer;
  DestA, DestR, DestB, DestG: Integer;
  SourceYStart, SourceXStart: Integer;
  SourceYEnd, SourceXEnd: Integer;
  AvgX, AvgY: Integer;
  ActualX: Integer;
  PixelsUsed: Integer;
  DestWidth, DestHeight, LBytesPerPixel: Integer;
begin
  DestWidth := StretchedBitmap.Width;
  DestHeight := StretchedBitmap.Height;
  if PixelFormat = pf24bit then
    LBytesPerPixel := 3
  else
    LBytesPerPixel := 4;

  SetLength(ScanLines, SourceBitmap.Height);
  for DestY := 0 to DestHeight - 1 do
  begin
    SourceYStart := Round(DestY / Scale);
    SourceYEnd := Round((DestY + 1) / Scale) - 1;

    if SourceYEnd >= SourceBitmap.Height then
      SourceYEnd := SourceBitmap.Height - 1;

    { Grab the destination pixels }
    DestLine := StretchedBitmap.ScanLine[DestY];
    for DestX := 0 to DestWidth - 1 do
    begin
      { Calculate the RGB value at this destination pixel }
      SourceXStart := Round(DestX / Scale);
      SourceXEnd := Round((DestX + 1) / Scale) - 1;

      DestA := 0;
      DestR := 0;
      DestB := 0;
      DestG := 0;
      PixelsUsed := 0;
      if SourceXEnd >= SourceBitmap.Width then
        SourceXEnd := SourceBitmap.Width - 1;
      for AvgY := SourceYStart to SourceYEnd do
      begin
        if ScanLines[AvgY] = nil then
          ScanLines[AvgY] := SourceBitmap.ScanLine[AvgY];
        CurrentLine := ScanLines[AvgY];
        for AvgX := SourceXStart to SourceXEnd do
        begin
          ActualX := AvgX * LBytesPerPixel;
{$IF DEFINED(CLR)}
          DestR := DestR + Marshal.ReadByte(CurrentLine, ActualX);
          DestB := DestB + Marshal.ReadByte(CurrentLine, ActualX + 1);
          DestG := DestG + Marshal.ReadByte(CurrentLine, ActualX + 2);
{$ELSE}
          DestR := DestR + CurrentLine[ActualX];
          DestB := DestB + CurrentLine[ActualX + 1];
          DestG := DestG + CurrentLine[ActualX + 2];
          if PixelFormat = pf32bit then
            DestA := DestA + CurrentLine[ActualX + 3];
{$ENDIF}
          Inc(PixelsUsed);
        end;
      end;

      ActualX := DestX * LBytesPerPixel;
{$IF DEFINED(CLR)}
      Marshal.WriteByte(DestLine, ActualX, Round(DestR / PixelsUsed));
      Marshal.WriteByte(DestLine, ActualX + 1, Round(DestB / PixelsUsed));
      Marshal.WriteByte(DestLine, ActualX + 2, Round(DestG / PixelsUsed));
{$ELSE}
      DestLine[ActualX] := Round(DestR / PixelsUsed);
      DestLine[ActualX + 1] := Round(DestB / PixelsUsed);
      DestLine[ActualX + 2] := Round(DestG / PixelsUsed);
      if PixelFormat = pf32bit then
        DestLine[ActualX + 3] := Round(DestA / PixelsUsed);
{$ENDIF}
    end;
  end;
end;

procedure EnlargeImage(const SourceBitmap, StretchedBitmap: TBitmap;
  Scale: Double; PixelFormat: TPixelFormat = pf24bit);
var
{$IF DEFINED(CLR)}
  ScanLines: array of IntPtr;
  DestLine: IntPtr;
  CurrentLine: IntPtr;
{$ELSE}
  ScanLines: array of PByteArray;
  DestLine: PByteArray;
  CurrentLine: PByteArray;
{$ENDIF}
  DestX, DestY: Integer;
  DestA, DestR, DestB, DestG: Double;
  SourceYStart, SourceXStart: Integer;
  SourceYPos: Integer;
  AvgX, AvgY: Integer;
  ActualX: Integer;
  { Use a 4 pixels for enlarging }
  XWeights, YWeights: array[0..1] of Double;
  PixelWeight: Double;
  DistFromStart: Double;
  DestWidth, DestHeight, LBytesPerPixel: Integer;
begin
  DestWidth := StretchedBitmap.Width;
  DestHeight := StretchedBitmap.Height;
  if PixelFormat = pf24bit then
    LBytesPerPixel := 3
  else
    LBytesPerPixel := 4;
  Scale := StretchedBitmap.Width / SourceBitmap.Width;
  SetLength(ScanLines, SourceBitmap.Height);
  for DestY := 0 to DestHeight - 1 do
  begin
    DistFromStart := DestY / Scale;
    SourceYStart := Round(DistFromSTart);
    YWeights[1] := DistFromStart - SourceYStart;
    if YWeights[1] < 0 then
      YWeights[1] := 0;
    YWeights[0] := 1 - YWeights[1];

    DestLine := StretchedBitmap.ScanLine[DestY];
    for DestX := 0 to DestWidth - 1 do
    begin
      { Calculate the RGB value at this destination pixel }
      DistFromStart := DestX / Scale;
      if DistFromStart > (SourceBitmap.Width - 1) then
        DistFromStart := SourceBitmap.Width - 1;
      SourceXStart := Round(DistFromStart);
      XWeights[1] := DistFromStart - SourceXStart;
      if XWeights[1] < 0 then
        XWeights[1] := 0;
      XWeights[0] := 1 - XWeights[1];

      { Average the four nearest pixels from the source mapped point }
      DestA := 0;
      DestR := 0;
      DestB := 0;
      DestG := 0;
      for AvgY := 0 to 1 do
      begin
        SourceYPos := SourceYStart + AvgY;
        if SourceYPos >= SourceBitmap.Height then
          SourceYPos := SourceBitmap.Height - 1;
        if ScanLines[SourceYPos] = nil then
          ScanLines[SourceYPos] := SourceBitmap.ScanLine[SourceYPos];
        CurrentLine := ScanLines[SourceYPos];

        for AvgX := 0 to 1 do
        begin
          if SourceXStart + AvgX >= SourceBitmap.Width then
            SourceXStart := SourceBitmap.Width - 1;

          if (AvgX = 1) and (SourceXStart = SourceBitmap.Width - 1) then
            Continue;

          ActualX := (SourceXStart + AvgX) * LBytesPerPixel;

          { Calculate how heavy this pixel is based on how far away
            it is from the mapped pixel }
          PixelWeight := XWeights[AvgX] * YWeights[AvgY];
{$IF DEFINED(CLR)}
          DestR := DestR + Marshal.ReadByte(CurrentLine, ActualX) * PixelWeight;
          DestB := DestB + Marshal.ReadByte(CurrentLine, ActualX + 1) * PixelWeight;
          DestG := DestG + Marshal.ReadByte(CurrentLine, ActualX + 2) * PixelWeight;
{$ELSE}
          DestR := DestR + CurrentLine[ActualX] * PixelWeight;
          DestB := DestB + CurrentLine[ActualX + 1] * PixelWeight;
          DestG := DestG + CurrentLine[ActualX + 2] * PixelWeight;
          if PixelFormat = pf32bit then
            DestA := DestA + CurrentLine[ActualX + 3] * PixelWeight;
{$ENDIF}
        end;
      end;

      ActualX := DestX * LBytesPerPixel;
{$IF DEFINED(CLR)}
      Marshal.WriteByte(DestLine, ActualX, Round(DestR));
      Marshal.WriteByte(DestLine, ActualX + 1, Round(DestB));
      Marshal.WriteByte(DestLine, ActualX + 2, Round(DestG));
{$ELSE}
      DestLine[ActualX] := Round(DestR);
      DestLine[ActualX + 1] := Round(DestB);
      DestLine[ActualX + 2] := Round(DestG);
      if PixelFormat = pf32bit then
        DestLine[ActualX + 3] := Round(DestA);
{$ENDIF}
    end;
  end;
end;

procedure ScaleImage(const SourceBitmap, ResizedBitmap: TBitmap;
  const ScaleAmount: Double; PixelFormat: TPixelFormat = pf24bit);
var
  DestWidth, DestHeight: Integer;
begin
  if not (PixelFormat in [pf24bit, pf32bit]) then
    raise EInvalidGraphic.Create(SInvalidScaleImagePixelFormat);

  DestWidth := Round(SourceBitmap.Width * ScaleAmount);
  DestHeight := Round(SourceBitmap.Height * ScaleAmount);
  { We must work in 24-bit or c to ensure the pixel layout for
    scanline is correct }
  SourceBitmap.PixelFormat := PixelFormat;

  ResizedBitmap.Width := DestWidth;
  ResizedBitmap.Height := DestHeight;
  ResizedBitmap.Canvas.Brush.Color := Vcl.Graphics.clNone;
  ResizedBitmap.Canvas.FillRect(Rect(0, 0, DestWidth, DestHeight));
  ResizedBitmap.PixelFormat := PixelFormat;

  if ResizedBitmap.Width < SourceBitmap.Width then
    ShrinkImage(SourceBitmap, ResizedBitmap, ScaleAmount, PixelFormat)
  else
    EnlargeImage(SourceBitmap, ResizedBitmap, ScaleAmount, PixelFormat);
end;

function ColorToWebColorStr(Color: TColor): string;
var
  RGB: Integer;
begin
  RGB := ColorToRGB(Color);
  Result := UpperCase(Format('#%.2x%.2x%.2x', [GetRValue(RGB),
    GetGValue(RGB), GetBValue(RGB)]));  { do not localize }
end;

function ColorToWebColorName(Color: TColor): string;
begin
  Result := RGBToWebColorName(ColorToRGB(Color));
end;

function WebColorToRGB(WebColor: Integer): Integer;
begin
  Result := StrToInt(Format('$%.2x%.2x%.2x', [GetRValue(WebColor),
    GetGValue(WebColor), GetBValue(WebColor)]));  { do not localize }
end;

function RGBToWebColorStr(RGB: Integer): string;
begin
  Result := UpperCase(Format('#%.2x%.2x%.2x', [GetRValue(RGB),
    GetGValue(RGB), GetBValue(RGB)]));  { do not localize }
end;

function RGBToWebColorName(RGB: Integer): string;
var
  I: Integer;
begin
  Result := RGBToWebColorStr(RGB);
  for I := 0 to Length(WebNamedColors) - 1 do
    if RGB = WebNamedColors[I].Value then
    begin
      Result := WebNamedColors[I].Name;
      exit;
    end;
end;

function WebColorNameToColor(WebColorName: string): TColor;
var
  I: Integer;
begin
  for I := 0 to Length(WebNamedColors) - 1 do
    if CompareText(WebColorName, WebNamedColors[I].Name) = 0 then
    begin
      Result := WebNamedColors[I].Value;
      Exit;
    end;
  raise Exception.Create(SInvalidColorString);
end;

const
  OffsetValue: array[Boolean] of Integer = (0,1);

function WebColorStrToColor(WebColor: string): TColor;
var
  I: Integer;
  Offset: Integer;
begin
  if (Length(WebColor) < 6) or (Length(WebColor) > 7) then
    raise Exception.Create(SInvalidColorString);
  for I := 1 to Length(WebColor) do
    if not CharInSet(WebColor[I], ['#', 'a'..'f', 'A'..'F', '0'..'9']) then    { do not localize }
      raise Exception.Create(SInvalidColorString);
  Offset := OffsetValue[Pos('#', WebColor) = 1];
  Result := RGB(StrToInt('$' + Copy(WebColor, 1 + Offset, 2)),                             { do not localize }
    StrToInt('$' + Copy(WebColor, 3 + Offset, 2)), StrToInt('$' + Copy(WebColor, 5 + Offset, 2)));  { do not localize }
end;

procedure SortColorArray(ColorArray: TColorArray; L, R: Integer; SortType: TColorArraySortType;
  Reverse: Boolean);

  function Compare(A, B: Integer): Integer;
  var
    H1, L1, S1: Word;
    H2, L2, S2: Word;
    R1, G1, B1: Word;
    R2, G2, B2: Word;
  begin
    Result := 0;
    if SortType in [stHue, stSaturation, stLuminance] then
    begin
      if Reverse then
      begin
        ColorRGBToHLS(ColorArray[A].Value, H1, L1, S1);
        ColorRGBToHLS(ColorArray[B].Value, H2, L2, S2);
      end
      else
      begin
        ColorRGBToHLS(ColorArray[A].Value, H2, L2, S2);
        ColorRGBToHLS(ColorArray[B].Value, H1, L1, S1);
      end;
      case SortType of
        stHue: Result := H2 - H1;
        stSaturation: Result := S2 - S1;
        stLuminance: Result := L2 - L1;
      end;
    end
    else
    begin
      if Reverse then
      begin
        R1 := GetRValue(ColorArray[A].Value);
        G1 := GetGValue(ColorArray[A].Value);
        B1 := GetBValue(ColorArray[A].Value);
        R2 := GetRValue(ColorArray[B].Value);
        G2 := GetGValue(ColorArray[B].Value);
        B2 := GetBValue(ColorArray[B].Value);
      end
      else
      begin
        R2 := GetRValue(ColorArray[A].Value);
        G2 := GetGValue(ColorArray[A].Value);
        B2 := GetBValue(ColorArray[A].Value);
        R1 := GetRValue(ColorArray[B].Value);
        G1 := GetGValue(ColorArray[B].Value);
        B1 := GetBValue(ColorArray[B].Value);
      end;
      case SortType of
        stRed: Result := R2 - R1;
        stGreen: Result := G2 - G1;
        stBlue: Result := B2 - B1;
        stCombo: Result := (R2 + G2 + B2) - (R1 + G1 + B1);
      end;
    end;
  end;

var
  I, J, P: Integer;
  WebColor: TIdentMapEntry;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while Compare(I, P) < 0 do Inc(I);
      while Compare(J, P) > 0 do Dec(J);
      if I <= J then
      begin
        WebColor := ColorArray[I];
        ColorArray[I] := ColorArray[J];
        ColorArray[J] := WebColor;
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then SortColorArray(ColorArray, L, J, SortType);
    L := I;
  until I >= R;
end;

procedure DrawTransparentBitmap(Source: TBitmap; Destination: TCanvas; DestRect: TRect; Opacity: Byte);
var
  BlendFunc: TBlendFunction;
begin
  BlendFunc.BlendOp := AC_SRC_OVER;
  BlendFunc.BlendFlags := 0;
  BlendFunc.SourceConstantAlpha := Opacity;

  if Source.PixelFormat = pf32bit then
    BlendFunc.AlphaFormat := AC_SRC_ALPHA
  else
    BlendFunc.AlphaFormat := 0;

  Winapi.Windows.AlphaBlend(Destination.Handle, DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
                     Source.Canvas.Handle, 0, 0, Source.Width, Source.Height, BlendFunc);
end;

procedure DrawTransparentBitmap(Source: TBitmap; SourceRect: TRect; Destination: TCanvas; DestRect: TRect; Opacity: Byte);
var
  BlendFunc: TBlendFunction;
begin
  BlendFunc.BlendOp := AC_SRC_OVER;
  BlendFunc.BlendFlags := 0;
  BlendFunc.SourceConstantAlpha := Opacity;

  if Source.PixelFormat = pf32bit then
    BlendFunc.AlphaFormat := AC_SRC_ALPHA
  else
    BlendFunc.AlphaFormat := 0;

  Winapi.Windows.AlphaBlend(Destination.Handle, DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
                     Source.Canvas.Handle, SourceRect.Left, SourceRect.Top, SourceRect.Right - SourceRect.Left, SourceRect.Bottom - SourceRect.Top, BlendFunc);
end;

type
  CardinalArray = array of Cardinal;

function SplitTransparentBitmap(Source: TBitmap; SourceRect: TRect): TBitmap;
var
  I: Integer;
{$IFDEF CLR}
  J: Integer;
  BitsMem: IntPtr;
{$ENDIF}
begin
  Result := TBitmap.Create;
  Result.SetSize(SourceRect.Right - SourceRect.Left, SourceRect.Bottom - SourceRect.Top);
  Result.PixelFormat := Source.PixelFormat;

  //Clear the resulting alpha and color values to 0 which essentially changes
  //DrawTransparentBitmap to a source copy
  if Result.PixelFormat = pf32bit then
  begin
{$IFNDEF CLR}
    for I := 0 to Result.Height - 1 do
      ZeroMemory(Result.ScanLine[I], Result.Width * 4);
{$ELSE}
    for I := 0 to Result.Height - 1 do
    begin
      BitsMem := Result.ScanLine[I];
      for J := 0 to Result.Width - 1 do
      begin
        Marshal.WriteInt32(BitsMem, J * 4, 0);
      end;
    end;
{$ENDIF}
  end;

  DrawTransparentBitmap(Source, SourceRect, Result.Canvas, Rect(0, 0, Result.Width, Result.Height), 255);
end;

function ColorBlendRGB(const From, &To : TColor; const Mu : Single) : TColor;
  // Linearly interpolate. Note invariant: Mu is between 0 and 1
  function Lerp(const A, B : Integer; const Mu : Single) : Integer;
  var
    InverseMu : Single;
  begin
    InverseMu := 1.0 - Mu;
    Result := Round(InverseMu * A + Mu * B);
  end;
begin
  const RealFrom : TRGBQuad = TRGBQuad(ColorToRGB(From));
  const RealTo : TRGBQuad = TRGBQuad(ColorToRGB(&To));
  TRGBQuad(Result).rgbRed := Lerp(RealFrom.rgbRed, RealTo.rgbRed, Mu);
  TRGBQuad(Result).rgbGreen := Lerp(RealFrom.rgbGreen, RealTo.rgbGreen, Mu);
  TRGBQuad(Result).rgbBlue := Lerp(RealFrom.rgbBlue, RealTo.rgbBlue, Mu);
  TRGBQuad(Result).rgbReserved := $0;
end;

{$IFDEF USE_ZLIB}
function LoadCompressedResourceBitmap(ResID: string): TBitmap;
var
  ResStream: TResourceStream;
  ZStream: TZDecompressionStream;
begin
  Result := TBitmap.Create;

  ResStream := TResourceStream.Create(HInstance, ResID, RT_RCDATA);
  ZStream := TZDecompressionStream.Create(ResStream);
  try
    Result.LoadFromStream(ZStream);
  finally
    ZStream.Free;
    ResStream.Free;
  end;
end;
{$ENDIF USE_ZLIB}

initialization
  CachedHighlightLum := 0;
  CachedHighlightColor := 0;
  CachedHighlight := 0;
  CachedShadowLum := 0;
  CachedShadowColor := 0;
  CachedShadow := 0;
  CachedColorValue := 0;
  CachedLumValue := 0;
  CachedColorAdjustLuma := 0;
end.
