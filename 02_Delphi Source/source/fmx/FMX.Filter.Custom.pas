{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Filter.Custom;

interface

{$SCOPEDENUMS ON}

implementation

uses
  System.Types, System.SysUtils, System.Math.Vectors, System.UITypes, System.Rtti, FMX.Filter, FMX.Types3D, FMX.Types;

type
  TAffineMatrix = packed record
    m11, m12, m31: Single;
    m21, m22, m32: Single;
    m13, m23, m33: Single;
  end;

  TAffineFilter = class(TFilter)
  protected
    FMatrix, FInvMatrix: TAffineMatrix;
    procedure CalcMatrix(W, H: Integer); virtual;
    procedure LoadShaders; override;
    procedure CalcSize(var W, H: Integer); override;
  public
    constructor Create; override;
    class function FilterAttr: TFilterRec; override;
  end;

  TPerspectiveFilter = class(TFilter)
  protected
    FMatrix, FInvMatrix: TAffineMatrix;
    procedure LoadShaders; override;
    procedure CalcSize(var W, H: Integer); override;
    procedure CalcMatrix(W, H: Integer); virtual;
  public
    class function FilterAttr: TFilterRec; override;
    constructor Create; override;
  end;

  TCropFilter = class(TFilter)
  protected
    procedure CalcSize(var W, H: Integer); override;
  public
    class function FilterAttr: TFilterRec; override;
    constructor Create; override;
  end;

  TGaussianBlurFilter = class(TFilter)
  protected
    procedure LoadShaders; override;
  public
    class function FilterAttr: TFilterRec; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TGlowFilter = class(TFilter)
  protected
    procedure LoadShaders; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function FilterAttr: TFilterRec; override;
  end;
  
  TInnerGlowFilter = class(TFilter)
  protected
    procedure LoadShaders; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function FilterAttr: TFilterRec; override;
  end;

  TSwipeFilter = class(TFilter)
  protected
    FAPoint: TPointF;
    FA1Point: TPointF;
    FBPoint: TPointF;
    FCPoint: TPointF;
    FLineAB: TPointF;
    FLineAB1: TPointF;
    FK: Single;
    FLength: Single;
    procedure LoadShaders; override;
    procedure CalcSize(var W, H: Integer); override;
  public
    constructor Create; override;
    class function FilterAttr: TFilterRec; override;
  end;
  
{ TAffineFilter }

const
  Epsilon: Single = 1e-40;
  cPIdiv180: Single =  0.017453292;
  IdentityMatrix: TAffineMatrix = (m11:1.0; m12:0.0; m31:0.0;
                                     m21:0.0; m22:1.0; m32:0.0;
                                     m13:0.0; m23:0.0; m33:1.0);

function AffineMatrixMultiply(const M1, M2: TAffineMatrix): TAffineMatrix;
begin
  Result.m11 := M1.m11 * M2.m11 + M1.m12 * M2.m21 + M1.m13 * M2.m31;
  Result.m12 := M1.m11 * M2.m12 + M1.m12 * M2.m22 + M1.m13 * M2.m32;
  Result.m13 := M1.m11 * M2.m13 + M1.m12 * M2.m23 + M1.m13 * M2.m33;
  Result.m21 := M1.m21 * M2.m11 + M1.m22 * M2.m21 + M1.m23 * M2.m31;
  Result.m22 := M1.m21 * M2.m12 + M1.m22 * M2.m22 + M1.m23 * M2.m32;
  Result.m23 := M1.m21 * M2.m13 + M1.m22 * M2.m23 + M1.m23 * M2.m33;
  Result.m31 := M1.m31 * M2.m11 + M1.m32 * M2.m21 + M1.m33 * M2.m31;
  Result.m32 := M1.m31 * M2.m12 + M1.m32 * M2.m22 + M1.m33 * M2.m32;
  Result.m33 := M1.m31 * M2.m13 + M1.m32 * M2.m23 + M1.m33 * M2.m33;
end;

function AffineCreateRotationMatrix(angle: Single): TAffineMatrix;
var
  cosine, sine: Single;
begin
  sine := sin(angle);
  cosine := cos(angle);

  Result := IdentityMatrix;
  Result.m11 := cosine;
  Result.m12 := sine;
  Result.m21 := -sine;
  Result.m22 := cosine;
end;

function AffinePointTransform(const V: TPointF; const M: TAffineMatrix): TPointF;
var
  z: Single;
begin
  Result.X := V.X * M.m11 + V.Y * M.m21 + M.m31;
  Result.Y := V.X * M.m12 + V.Y * M.m22 + M.m32;
  z := M.m13 * V.x + M.m23 * V.y + 1;
  if z = 0 then Exit;
  if z = 1 then
  begin
    Result.X := V.X * M.m11 + V.Y * M.m21 + M.m31;
    Result.Y := V.X * M.m12 + V.Y * M.m22 + M.m32;
  end
  else
  begin
    z := 1 / z;
    Result.X := (V.X * M.m11 + V.Y * M.m21 + M.m31) * z;
    Result.Y := (V.X * M.m12 + V.Y * M.m22 + M.m32) * z;
  end;
end;

function AffineShaderMatrixDeterminant(const M: TAffineMatrix): Single;
begin
  Result := M.m11 * (M.m22 * M.m33 - M.m32 * M.m23)
          - M.m12 * (M.m21 * M.m33 - M.m31 * M.m23)
          + M.m13 * (M.m21 * M.m32 - M.m31 * M.m22);
end;

procedure AffineAdjointMatrix(var M: TAffineMatrix);
var
   a1, a2, a3,
   b1, b2, b3,
   c1, c2, c3: Single;
begin
   a1:= M.m11; a2:= M.m12; a3 := M.m13;
   b1:= M.m21; b2:= M.m22; b3 := M.m23;
   c1:= M.m31; c2:= M.m32; c3 := M.m33;
   M.m11 := (b2*c3-c2*b3);
   M.m21 :=-(b1*c3-c1*b3);
   M.m31 := (b1*c2-c1*b2);

   M.m12 :=-(a2*c3-c2*a3);
   M.m22 := (a1*c3-c1*a3);
   M.m32 :=-(a1*c2-c1*a2);

   M.m13 := (a2*b3-b2*a3);
   M.m23 :=-(a1*b3-b1*a3);
   M.m33 := (a1*b2-b1*a2);
end;

procedure AffineScaleMatrix(var M: TAffineMatrix; const factor: Single);
begin
  M.m11 := M.m11 * Factor;
  M.m12 := M.m12 * Factor;
  M.m21 := M.m21 * Factor;
  M.m22 := M.m22 * Factor;
  M.m31 := M.m31 * Factor;
  M.m32 := M.m32 * Factor;
  M.m13 := M.m13 * Factor;
  M.m23 := M.m23 * Factor;
  M.m33 := M.m33 * Factor;
end;

procedure AffineInvertMatrix(var M: TAffineMatrix);
var
   det : Single;
begin
  det := AffineShaderMatrixDeterminant(M);
  if Abs(Det) < EPSILON then
     M := IdentityMatrix
  else
  begin
    AffineAdjointMatrix(M);
    AffineScaleMatrix(M, 1/det);
  end;
end;

constructor TAffineFilter.Create;
begin
  inherited;
  FAntiAliasing := True;
  FShaders[FPassCount - 1] := TShaderManager.RegisterShaderFromData('affine.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.SKSL'}
    TContextShaderSource.Create(TContextShaderArch.SKSL, [
      $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $68, $61, $64, $65, $72, 
      $20, $49, $6E, $70, $75, $74, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, 
      $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $33, $20, $4D, $61, $74, $72, $69, $78, $31, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $33, $20, $4D, $61, $74, $72, 
      $69, $78, $32, $3B, $76, $6F, $69, $64, $20, $61, $28, $69, $6E, $6F, $75, $74, $20, $66, $6C, $6F, $61, $74, $32, $20, $62, $29, $7B, $62, $3D, $66, $6C, $6F, $61, $74, $32, $28, $28, $62, $2E, $78, 
      $2A, $4D, $61, $74, $72, $69, $78, $31, $2E, $78, $2B, $62, $2E, $79, $2A, $4D, $61, $74, $72, $69, $78, $31, $2E, $79, $29, $2B, $4D, $61, $74, $72, $69, $78, $31, $2E, $7A, $2C, $28, $62, $2E, $78, 
      $2A, $4D, $61, $74, $72, $69, $78, $32, $2E, $78, $2B, $62, $2E, $79, $2A, $4D, $61, $74, $72, $69, $78, $32, $2E, $79, $29, $2B, $4D, $61, $74, $72, $69, $78, $32, $2E, $7A, $29, $3B, $7D, $68, $61, 
      $6C, $66, $34, $20, $6D, $61, $69, $6E, $28, $66, $6C, $6F, $61, $74, $32, $20, $62, $29, $7B, $66, $6C, $6F, $61, $74, $32, $20, $63, $3D, $62, $2F, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, 
      $3B, $61, $28, $63, $29, $3B, $62, $6F, $6F, $6C, $20, $64, $3D, $28, $28, $63, $2E, $78, $3E, $3D, $30, $2E, $26, $26, $63, $2E, $78, $3C, $3D, $31, $2E, $29, $26, $26, $63, $2E, $79, $3E, $3D, $30, 
      $2E, $29, $26, $26, $63, $2E, $79, $3C, $3D, $31, $2E, $3B, $72, $65, $74, $75, $72, $6E, $20, $68, $61, $6C, $66, $34, $28, $64, $3F, $66, $6C, $6F, $61, $74, $34, $28, $49, $6E, $70, $75, $74, $2E, 
      $65, $76, $61, $6C, $28, $63, $2A, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $29, $29, $3A, $66, $6C, $6F, $61, $74, $34, $28, $30, $2E, $29, $29, $3B, $7D, $0A, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Resolution', TContextShaderVariableKind.Float2, 0, 8),
      TContextShaderVariable.Create('InputResolution', TContextShaderVariableKind.Float2, 1, 8),
      TContextShaderVariable.Create('Matrix1', TContextShaderVariableKind.Float3, 2, 12),
      TContextShaderVariable.Create('Matrix2', TContextShaderVariableKind.Float3, 3, 12)]
    ),
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FF, $FF, $FE, $FF, $30, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $97, $00, $00, $00, $00, $02, $FF, $FF, $03, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $90, $00, $00, $00,
      $58, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $60, $00, $00, $00, $00, $00, $00, $00, $70, $00, $00, $00, $02, $00, $00, $00, $01, $00, $00, $00, $78, $00, $00, $00, $00, $00, $00, $00,
      $88, $00, $00, $00, $02, $00, $01, $00, $01, $00, $00, $00, $78, $00, $00, $00, $00, $00, $00, $00, $49, $6E, $70, $75, $74, $00, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00,
      $00, $00, $00, $00, $4D, $61, $74, $72, $69, $78, $31, $00, $01, $00, $03, $00, $01, $00, $03, $00, $01, $00, $00, $00, $00, $00, $00, $00, $4D, $61, $74, $72, $69, $78, $32, $00, $70, $73, $5F, $32,
      $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00,
      $51, $00, $00, $05, $02, $00, $0F, $A0, $00, $00, $80, $3F, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02,
      $00, $00, $00, $90, $00, $08, $0F, $A0, $05, $00, $00, $03, $00, $00, $08, $80, $00, $00, $55, $B0, $00, $00, $55, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $00, $00, $00, $B0, $00, $00, $00, $A0,
      $00, $00, $FF, $80, $02, $00, $00, $03, $00, $00, $01, $80, $00, $00, $00, $80, $00, $00, $AA, $A0, $02, $00, $00, $03, $00, $00, $04, $80, $00, $00, $00, $81, $02, $00, $00, $A0, $58, $00, $00, $04,
      $00, $00, $04, $80, $00, $00, $AA, $80, $02, $00, $00, $A0, $02, $00, $55, $A0, $58, $00, $00, $04, $00, $00, $08, $80, $00, $00, $00, $80, $02, $00, $00, $A0, $02, $00, $55, $A0, $05, $00, $00, $03,
      $00, $00, $04, $80, $00, $00, $AA, $80, $00, $00, $FF, $80, $05, $00, $00, $03, $00, $00, $08, $80, $00, $00, $55, $B0, $01, $00, $55, $A0, $04, $00, $00, $04, $00, $00, $08, $80, $00, $00, $00, $B0,
      $01, $00, $00, $A0, $00, $00, $FF, $80, $02, $00, $00, $03, $00, $00, $02, $80, $00, $00, $FF, $80, $01, $00, $AA, $A0, $58, $00, $00, $04, $00, $00, $08, $80, $00, $00, $55, $80, $02, $00, $00, $A0,
      $02, $00, $55, $A0, $05, $00, $00, $03, $00, $00, $04, $80, $00, $00, $AA, $80, $00, $00, $FF, $80, $02, $00, $00, $03, $00, $00, $08, $80, $00, $00, $55, $81, $02, $00, $00, $A0, $42, $00, $00, $03,
      $01, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $58, $00, $00, $04, $00, $00, $01, $80, $00, $00, $FF, $80, $02, $00, $00, $A0, $02, $00, $55, $A0, $05, $00, $00, $03, $00, $00, $01, $80,
      $00, $00, $AA, $80, $00, $00, $00, $80, $58, $00, $00, $04, $00, $00, $0F, $80, $00, $00, $00, $81, $02, $00, $55, $A0, $01, $00, $E4, $80, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80,
      $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Matrix1', TContextShaderVariableKind.Vector, 0, 1),
      TContextShaderVariable.Create('Matrix2', TContextShaderVariableKind.Vector, 1, 1)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX11_level_9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $C4, $29, $2E, $3F, $7B, $B0, $BB, $4D, $4F, $45, $F8, $A8, $79, $17, $4A, $57, $01, $00, $00, $00, $DC, $05, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $B4, $01, $00, $00,
      $78, $03, $00, $00, $F4, $03, $00, $00, $74, $05, $00, $00, $A8, $05, $00, $00, $41, $6F, $6E, $39, $74, $01, $00, $00, $74, $01, $00, $00, $00, $02, $FF, $FF, $40, $01, $00, $00, $34, $00, $00, $00,
      $01, $00, $28, $00, $00, $00, $34, $00, $00, $00, $34, $00, $01, $00, $24, $00, $00, $00, $34, $00, $00, $00, $00, $00, $00, $00, $01, $00, $02, $00, $00, $00, $00, $00, $00, $00, $00, $02, $FF, $FF,
      $51, $00, $00, $05, $02, $00, $0F, $A0, $00, $00, $80, $3F, $00, $00, $80, $BF, $00, $00, $00, $80, $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02,
      $00, $00, $00, $90, $00, $08, $0F, $A0, $05, $00, $00, $03, $00, $00, $08, $80, $00, $00, $55, $B0, $01, $00, $55, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $00, $00, $00, $B0, $01, $00, $00, $A0,
      $00, $00, $FF, $80, $02, $00, $00, $03, $00, $00, $02, $80, $00, $00, $00, $80, $01, $00, $AA, $A0, $02, $00, $00, $03, $00, $00, $04, $80, $00, $00, $55, $81, $02, $00, $00, $A0, $58, $00, $00, $04,
      $00, $00, $04, $80, $00, $00, $AA, $80, $02, $00, $55, $A0, $02, $00, $AA, $A0, $58, $00, $00, $04, $00, $00, $04, $80, $00, $00, $55, $80, $00, $00, $AA, $80, $02, $00, $AA, $A0, $05, $00, $00, $03,
      $00, $00, $08, $80, $00, $00, $55, $B0, $00, $00, $55, $A0, $04, $00, $00, $04, $00, $00, $08, $80, $00, $00, $00, $B0, $00, $00, $00, $A0, $00, $00, $FF, $80, $02, $00, $00, $03, $00, $00, $01, $80,
      $00, $00, $FF, $80, $00, $00, $AA, $A0, $02, $00, $00, $03, $00, $00, $08, $80, $00, $00, $00, $81, $02, $00, $00, $A0, $58, $00, $00, $04, $00, $00, $04, $80, $00, $00, $FF, $80, $00, $00, $AA, $80,
      $02, $00, $AA, $A0, $58, $00, $00, $04, $00, $00, $04, $80, $00, $00, $00, $80, $00, $00, $AA, $80, $02, $00, $AA, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0,
      $58, $00, $00, $04, $00, $00, $0F, $80, $00, $00, $AA, $80, $02, $00, $FF, $A0, $01, $00, $E4, $80, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00, $53, $48, $44, $52,
      $BC, $01, $00, $00, $40, $00, $00, $00, $6F, $00, $00, $00, $59, $00, $00, $04, $46, $8E, $20, $00, $00, $00, $00, $00, $03, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $00, $00, $00, $00,
      $58, $18, $00, $04, $00, $70, $10, $00, $00, $00, $00, $00, $55, $55, $00, $00, $62, $10, $00, $03, $32, $10, $10, $00, $00, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00, $00, $00, $00, $00,
      $68, $00, $00, $02, $03, $00, $00, $00, $0F, $00, $00, $08, $12, $00, $10, $00, $00, $00, $00, $00, $46, $10, $10, $00, $00, $00, $00, $00, $46, $80, $20, $00, $00, $00, $00, $00, $01, $00, $00, $00,
      $00, $00, $00, $08, $12, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00, $00, $00, $00, $00, $2A, $80, $20, $00, $00, $00, $00, $00, $01, $00, $00, $00, $0F, $00, $00, $08, $42, $00, $10, $00,
      $00, $00, $00, $00, $46, $10, $10, $00, $00, $00, $00, $00, $46, $80, $20, $00, $00, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $08, $22, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00,
      $00, $00, $00, $00, $2A, $80, $20, $00, $00, $00, $00, $00, $02, $00, $00, $00, $1D, $00, $00, $0A, $C2, $00, $10, $00, $00, $00, $00, $00, $06, $04, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $1D, $00, $00, $0A, $32, $00, $10, $00, $01, $00, $00, $00, $02, $40, $00, $00, $00, $00, $80, $3F, $00, $00, $80, $3F,
      $00, $00, $00, $00, $00, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $45, $00, $00, $09, $F2, $00, $10, $00, $02, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00,
      $00, $00, $00, $00, $00, $60, $10, $00, $00, $00, $00, $00, $01, $00, $00, $07, $12, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00, $01, $00, $00, $00,
      $01, $00, $00, $07, $12, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00, $00, $00, $00, $00, $01, $00, $00, $07, $12, $00, $10, $00, $00, $00, $00, $00,
      $1A, $00, $10, $00, $01, $00, $00, $00, $0A, $00, $10, $00, $00, $00, $00, $00, $01, $00, $00, $07, $F2, $20, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $02, $00, $00, $00, $06, $00, $10, $00,
      $00, $00, $00, $00, $3E, $00, $00, $01, $53, $54, $41, $54, $74, $00, $00, $00, $0C, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $02, $00, $00, $00, $06, $00, $00, $00, $00, $00, $00, $00,
      $04, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $52, $44, $45, $46, $78, $01, $00, $00, $01, $00, $00, $00, $8C, $00, $00, $00, $03, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FF, $FF,
      $00, $11, $00, $00, $44, $01, $00, $00, $7C, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00,
      $7C, $00, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $01, $00, $00, $00, $0C, $00, $00, $00, $82, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $49, $6E, $70, $75, $74, $00, $24, $47, $6C, $6F, $62, $61, $6C, $73, $00, $AB,
      $82, $00, $00, $00, $04, $00, $00, $00, $A4, $00, $00, $00, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $04, $01, $00, $00, $00, $00, $00, $00, $04, $00, $00, $00, $00, $00, $00, $00,
      $0C, $01, $00, $00, $00, $00, $00, $00, $1C, $01, $00, $00, $04, $00, $00, $00, $04, $00, $00, $00, $00, $00, $00, $00, $0C, $01, $00, $00, $00, $00, $00, $00, $23, $01, $00, $00, $10, $00, $00, $00,
      $0C, $00, $00, $00, $02, $00, $00, $00, $2C, $01, $00, $00, $00, $00, $00, $00, $3C, $01, $00, $00, $20, $00, $00, $00, $0C, $00, $00, $00, $02, $00, $00, $00, $2C, $01, $00, $00, $00, $00, $00, $00,
      $57, $69, $64, $74, $68, $00, $AB, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $48, $65, $69, $67, $68, $74, $00, $4D, $61, $74, $72, $69, $78, $31, $00, $AB,
      $01, $00, $03, $00, $01, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $4D, $61, $74, $72, $69, $78, $32, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $48, $4C,
      $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E, $39, $35, $32, $2E, $32, $38, $34, $34, $00, $AB, $AB, $AB, $49, $53, $47, $4E,
      $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $03, $03, $00, $00, $54, $45, $58, $43,
      $4F, $4F, $52, $44, $00, $AB, $AB, $AB, $4F, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00,
      $00, $00, $00, $00, $0F, $00, $00, $00, $53, $56, $5F, $54, $61, $72, $67, $65, $74, $00, $AB, $AB], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 4),
      TContextShaderVariable.Create('Height', TContextShaderVariableKind.Float, 4, 4),
      TContextShaderVariable.Create('Matrix1', TContextShaderVariableKind.Vector, 16, 12),
      TContextShaderVariable.Create('Matrix2', TContextShaderVariableKind.Vector, 32, 12)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.Metal'}
    {$IF defined(MACOS)}
    TContextShaderSource.Create(
      TContextShaderArch.Metal,
      TEncoding.UTF8.GetBytes(
        'using namespace metal;'+

        'struct ProjectedVertex {'+
          'float4 position [[position]];'+
          'float2 textureCoord;'+
        '};'+

        'float2 applymatrix(float2 val, float3x2 mat)'+
        '{'+
          'float2 newval;'+
          'newval.x = val.x * mat[0][0] + val.y * mat[1][0] + mat[2][0];'+
          'newval.y = val.x * mat[0][1] + val.y * mat[1][1] + mat[2][1];'+
          'return newval;'+
        '}'+

        'fragment float4 fragmentShader(const ProjectedVertex in [[stage_in]],'+
                                       'constant float4 &Matrix1 [[buffer(0)]],'+
                                       'constant float4 &Matrix2 [[buffer(1)]],'+
                                       'const texture2d<float> Input [[texture(0)]],'+
                                       'const sampler InputSampler [[sampler(0)]]) {'+
          'float3x2 Matrix;'+
          'Matrix[0][0] = Matrix1[0];'+
          'Matrix[1][0] = Matrix1[1];'+
          'Matrix[2][0] = Matrix1[2];'+
          'Matrix[0][1] = Matrix2[0];'+
          'Matrix[1][1] = Matrix2[1];'+
          'Matrix[2][1] = Matrix2[2];'+
          'float2 newCoord = in.textureCoord;'+
          'newCoord = applymatrix(newCoord, Matrix);'+
          'bool isValid = (newCoord.x >= 0 && newCoord.x <= 1 && newCoord.y >= 0 && newCoord.y <= 1);'+
          'return isValid ? Input.sample(InputSampler, newCoord) : float4(0, 0, 0, 0);'+
        '}'
      ),
      [TContextShaderVariable.Create('Matrix1', TContextShaderVariableKind.Float3, 0, 1),
       TContextShaderVariable.Create('Matrix2', TContextShaderVariableKind.Float3, 1, 1),
       TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.GLSL'}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $30, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $6E,
      $65, $77, $76, $61, $6C, $30, $30, $30, $38, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $63, $30, $30, $31, $30, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $61, $6D, $70, $6C, $65,
      $72, $32, $44, $20, $5F, $49, $6E, $70, $75, $74, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $33, $20, $5F, $4D, $61, $74, $72, $69, $78, $31, $3B, $0D, $0A, $75, $6E, $69,
      $66, $6F, $72, $6D, $20, $76, $65, $63, $33, $20, $5F, $4D, $61, $74, $72, $69, $78, $32, $3B, $0D, $0A, $76, $6F, $69, $64, $20, $6D, $61, $69, $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20,
      $20, $76, $65, $63, $32, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $3B, $0D, $0A, $20, $20, $20, $20, $62, $6F, $6F, $6C, $20, $5F, $69, $73, $56, $61, $6C, $69, $64, $3B, $0D, $0A, $20, $20,
      $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $76, $65, $63, $32, $28, $54, $45, $58, $30, $2E, $78, $2C, $20, $31, $2E, $30, $20, $2D, $20, $54, $45, $58, $30, $2E, $79, $29,
      $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $76, $61, $6C, $30, $30, $30, $38, $2E, $78, $20, $3D, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $2A, $5F, $4D, $61, $74, $72,
      $69, $78, $31, $2E, $78, $20, $2B, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $2A, $5F, $4D, $61, $74, $72, $69, $78, $31, $2E, $79, $20, $2B, $20, $5F, $4D, $61, $74, $72, $69, $78,
      $31, $2E, $7A, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $76, $61, $6C, $30, $30, $30, $38, $2E, $79, $20, $3D, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $2A, $5F, $4D,
      $61, $74, $72, $69, $78, $32, $2E, $78, $20, $2B, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $2A, $5F, $4D, $61, $74, $72, $69, $78, $32, $2E, $79, $20, $2B, $20, $5F, $4D, $61, $74,
      $72, $69, $78, $32, $2E, $7A, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $69, $73, $56, $61, $6C, $69, $64, $20, $3D, $20, $5F, $6E, $65, $77, $76, $61, $6C, $30, $30, $30, $38, $2E, $78, $20, $3E, $3D,
      $20, $30, $2E, $30, $20, $26, $26, $20, $5F, $6E, $65, $77, $76, $61, $6C, $30, $30, $30, $38, $2E, $78, $20, $3C, $3D, $20, $31, $2E, $30, $20, $26, $26, $20, $5F, $6E, $65, $77, $76, $61, $6C, $30,
      $30, $30, $38, $2E, $79, $20, $3E, $3D, $20, $30, $2E, $30, $20, $26, $26, $20, $5F, $6E, $65, $77, $76, $61, $6C, $30, $30, $30, $38, $2E, $79, $20, $3C, $3D, $20, $31, $2E, $30, $3B, $0D, $0A, $20,
      $20, $20, $20, $69, $66, $20, $28, $5F, $69, $73, $56, $61, $6C, $69, $64, $29, $20, $7B, $20, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $63, $30, $30, $31, $30, $20, $3D, $20, $76, $65,
      $63, $32, $28, $5F, $6E, $65, $77, $76, $61, $6C, $30, $30, $30, $38, $2E, $78, $2C, $20, $31, $2E, $30, $20, $2D, $20, $5F, $6E, $65, $77, $76, $61, $6C, $30, $30, $30, $38, $2E, $79, $29, $3B, $0D,
      $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $30, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $63, $30, $30, $31,
      $30, $29, $3B, $0D, $0A, $20, $20, $20, $20, $7D, $20, $65, $6C, $73, $65, $20, $7B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $30, $20, $3D, $20, $76, $65, $63, $34, $28,
      $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $3B, $0D, $0A, $20, $20, $20, $20, $7D, $20, $2F, $2F, $20, $65, $6E, $64, $20, $69, $66, $0D, $0A,
      $20, $20, $20, $20, $67, $6C, $5F, $46, $72, $61, $67, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $54, $4D, $50, $30, $3B, $0D, $0A, $20, $20, $20, $20, $72, $65, $74, $75, $72, $6E, $3B, $0D, $0A,
      $7D, $20, $0D, $0A], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Matrix1', TContextShaderVariableKind.Float3, 0, 1),
      TContextShaderVariable.Create('Matrix2', TContextShaderVariableKind.Float3, 0, 1)]
    )
    {$ENDREGION}

  ]);
end;

procedure TAffineFilter.CalcSize(var W, H: Integer);
var
  P: TPointF;
  W1, H1, WW, HH: Single;
  S: TAffineMatrix;
begin
  CalcMatrix(InputSize.Width, InputSize.Height);
  W1 := -100;
  H1 := -100;
  WW := 100;
  HH := 100;
  P.x := 1; P.y := 1;
  P := AffinePointTransform(P, FMatrix);
  if P.x > W1 then W1 := P.x;
  if P.y > H1 then H1 := P.y;
  if P.x < WW then WW := P.x;
  if P.y < HH then HH := P.y;
  P.x := 0; P.y := 1;
  P := AffinePointTransform(P, FMatrix);
  if P.x > W1 then W1 := P.x;
  if P.y > H1 then H1 := P.y;
  if P.x < WW then WW := P.x;
  if P.y < HH then HH := P.y;
  P.x := 0; P.y := 0;
  P := AffinePointTransform(P, FMatrix);
  if P.x > W1 then W1 := P.x;
  if P.y > H1 then H1 := P.y;
  if P.x < WW then WW := P.x;
  if P.y < HH then HH := P.y;
  P.x := 1; P.y := 0;
  P := AffinePointTransform(P, FMatrix);
  if P.x > W1 then W1 := P.x;
  if P.y > H1 then H1 := P.y;
  if P.x < WW then WW := P.x;
  if P.y < HH then HH := P.y;
  W := round((W1 - WW) * InputSize.Width);
  H := round((H1 - HH) * InputSize.Height);
  // Recalc matrix
  S := IdentityMatrix;
  if W = 0 then
    S.m11 := 0
  else
    S.m11 := InputSize.Width / W;
  if H = 0 then
    S.m22 := 0
  else
    S.m22 := InputSize.Height / H;
  S.m31 := -WW * S.m11;
  S.m32 := -HH * S.m22;
  FMatrix := AffineMatrixMultiply(FMatrix, S);
  FInvMatrix := FMatrix;
  AffineInvertMatrix(FInvMatrix);
end;

procedure TAffineFilter.CalcMatrix(W, H: Integer);
var
  S, US, T, R, UT: TAffineMatrix;
begin
  S := IdentityMatrix;
  S.m11 := W;
  S.m22 := H;
  T := IdentityMatrix;
  T.m31 := -ValuesAsPoint['Center'].X / InputSize.Width;
  T.m32 := -ValuesAsPoint['Center'].Y / InputSize.Height;
  R := AffineCreateRotationMatrix(ValuesAsFloat['Rotation'] * cPIdiv180);
  UT := IdentityMatrix;
  UT.m31 := ValuesAsPoint['Center'].X / InputSize.Width;
  UT.m32 := ValuesAsPoint['Center'].Y / InputSize.Height;
  US := IdentityMatrix;
  US.m11 := 1 / W;
  US.m22 := 1 / H;
  FMatrix := AffineMatrixMultiply(T, S);
  FMatrix := AffineMatrixMultiply(FMatrix, R);
  FMatrix := AffineMatrixMultiply(FMatrix, US);
  FMatrix := AffineMatrixMultiply(FMatrix, UT);
  S := IdentityMatrix;
  S.m11 := ValuesAsFloat['Scale'];
  S.m22 := ValuesAsFloat['Scale'];
  FMatrix := AffineMatrixMultiply(FMatrix, S);
end;

procedure TAffineFilter.LoadShaders;
begin
  FilterContext.SetShaders(FVertexShader, FShaders[FPass]);
  FilterContext.SetShaderVariable('Matrix1', [Vector3D(FInvMatrix.m11, FInvMatrix.m21, FInvMatrix.m31, 0)]);
  FilterContext.SetShaderVariable('Matrix2', [Vector3D(FInvMatrix.m12, FInvMatrix.m22, FInvMatrix.m32, 0)]);
end;

class function TAffineFilter.FilterAttr: TFilterRec;
begin
  Result := TFilterRec.Create('AffineTransform', 'Applies an affine transform to an image.', [
    TFilterValueRec.Create('Center', 'The center point of the rotation.', TFilterValueType.Point, TValue.From<TPointF>(TPointF.Create(150, 150)), 
      TValue.From<TPointF>(TPointF.Zero), TValue.From<TPointF>(TPointF.Create(65535, 65535))),
    TFilterValueRec.Create('Rotation', 'Rotation angle in degrees.', TFilterValueType.Float, 0, -180, 180),
    TFilterValueRec.Create('Scale', 'Scale value as floating.', TFilterValueType.Float, 1, 0.05, 4)
  ]);
end;

{ TPerspectiveFilter }

constructor TPerspectiveFilter.Create;
begin
  inherited;
  FAntiAliasing := True;
  FShaders[FPassCount - 1] := TShaderManager.RegisterShaderFromData('projective.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.SKSL'}
    TContextShaderSource.Create(TContextShaderArch.SKSL, [
      $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $68, $61, $64, $65, $72, 
      $20, $49, $6E, $70, $75, $74, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, 
      $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $33, $20, $4D, $61, $74, $72, $69, $78, $31, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $33, $20, $4D, $61, $74, $72, 
      $69, $78, $32, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $33, $20, $4D, $61, $74, $72, $69, $78, $33, $3B, $66, $6C, $6F, $61, $74, $32, $20, $61, $28, $63, $6F, $6E, $73, 
      $74, $20, $66, $6C, $6F, $61, $74, $32, $20, $62, $29, $7B, $66, $6C, $6F, $61, $74, $32, $20, $63, $3B, $66, $6C, $6F, $61, $74, $20, $64, $3D, $28, $4D, $61, $74, $72, $69, $78, $33, $2E, $78, $2A, 
      $62, $2E, $78, $2B, $4D, $61, $74, $72, $69, $78, $33, $2E, $79, $2A, $62, $2E, $79, $29, $2B, $4D, $61, $74, $72, $69, $78, $33, $2E, $7A, $3B, $69, $66, $28, $64, $3D, $3D, $31, $2E, $29, $7B, $63, 
      $2E, $78, $3D, $28, $62, $2E, $78, $2A, $4D, $61, $74, $72, $69, $78, $31, $2E, $78, $2B, $62, $2E, $79, $2A, $4D, $61, $74, $72, $69, $78, $31, $2E, $79, $29, $2B, $4D, $61, $74, $72, $69, $78, $31, 
      $2E, $7A, $3B, $63, $2E, $79, $3D, $28, $62, $2E, $78, $2A, $4D, $61, $74, $72, $69, $78, $32, $2E, $78, $2B, $62, $2E, $79, $2A, $4D, $61, $74, $72, $69, $78, $32, $2E, $79, $29, $2B, $4D, $61, $74, 
      $72, $69, $78, $32, $2E, $7A, $3B, $7D, $65, $6C, $73, $65, $7B, $64, $3D, $31, $2E, $2F, $64, $3B, $63, $2E, $78, $3D, $28, $28, $62, $2E, $78, $2A, $4D, $61, $74, $72, $69, $78, $31, $2E, $78, $2B, 
      $62, $2E, $79, $2A, $4D, $61, $74, $72, $69, $78, $31, $2E, $79, $29, $2B, $4D, $61, $74, $72, $69, $78, $31, $2E, $7A, $29, $2A, $64, $3B, $63, $2E, $79, $3D, $28, $28, $62, $2E, $78, $2A, $4D, $61, 
      $74, $72, $69, $78, $32, $2E, $78, $2B, $62, $2E, $79, $2A, $4D, $61, $74, $72, $69, $78, $32, $2E, $79, $29, $2B, $4D, $61, $74, $72, $69, $78, $32, $2E, $7A, $29, $2A, $64, $3B, $7D, $72, $65, $74, 
      $75, $72, $6E, $20, $63, $3B, $7D, $68, $61, $6C, $66, $34, $20, $6D, $61, $69, $6E, $28, $66, $6C, $6F, $61, $74, $32, $20, $62, $29, $7B, $66, $6C, $6F, $61, $74, $32, $20, $63, $3D, $62, $2F, $52, 
      $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $63, $3D, $61, $28, $63, $29, $3B, $62, $6F, $6F, $6C, $20, $64, $3D, $28, $28, $63, $2E, $78, $3E, $3D, $30, $2E, $26, $26, $63, $2E, $78, $3C, $3D, 
      $31, $2E, $29, $26, $26, $63, $2E, $79, $3E, $3D, $30, $2E, $29, $26, $26, $63, $2E, $79, $3C, $3D, $31, $2E, $3B, $72, $65, $74, $75, $72, $6E, $20, $68, $61, $6C, $66, $34, $28, $64, $3F, $66, $6C, 
      $6F, $61, $74, $34, $28, $49, $6E, $70, $75, $74, $2E, $65, $76, $61, $6C, $28, $63, $2A, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $29, $29, $3A, $66, $6C, $6F, $61, 
      $74, $34, $28, $30, $2E, $29, $29, $3B, $7D, $0A, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Resolution', TContextShaderVariableKind.Float2, 0, 8),
      TContextShaderVariable.Create('InputResolution', TContextShaderVariableKind.Float2, 1, 8),
      TContextShaderVariable.Create('Matrix1', TContextShaderVariableKind.Float3, 2, 12),
      TContextShaderVariable.Create('Matrix2', TContextShaderVariableKind.Float3, 3, 12),
      TContextShaderVariable.Create('Matrix3', TContextShaderVariableKind.Float3, 4, 12)]
    ),
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FF, $FF, $FE, $FF, $37, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $B3, $00, $00, $00, $00, $02, $FF, $FF, $04, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $AC, $00, $00, $00,
      $6C, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $74, $00, $00, $00, $00, $00, $00, $00, $84, $00, $00, $00, $02, $00, $00, $00, $01, $00, $00, $00, $8C, $00, $00, $00, $00, $00, $00, $00,
      $9C, $00, $00, $00, $02, $00, $01, $00, $01, $00, $00, $00, $8C, $00, $00, $00, $00, $00, $00, $00, $A4, $00, $00, $00, $02, $00, $02, $00, $01, $00, $00, $00, $8C, $00, $00, $00, $00, $00, $00, $00,
      $49, $6E, $70, $75, $74, $00, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $4D, $61, $74, $72, $69, $78, $31, $00, $01, $00, $03, $00, $01, $00, $03, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $4D, $61, $74, $72, $69, $78, $32, $00, $4D, $61, $74, $72, $69, $78, $33, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74,
      $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $51, $00, $00, $05, $03, $00, $0F, $A0, $00, $00, $80, $BF,
      $00, $00, $80, $3F, $00, $00, $00, $00, $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $05, $00, $00, $03,
      $00, $00, $08, $80, $00, $00, $55, $B0, $02, $00, $55, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $00, $A0, $00, $00, $00, $B0, $00, $00, $FF, $80, $02, $00, $00, $03, $00, $00, $01, $80,
      $00, $00, $00, $80, $02, $00, $AA, $A0, $02, $00, $00, $03, $00, $00, $02, $80, $00, $00, $00, $80, $03, $00, $00, $A0, $06, $00, $00, $02, $00, $00, $01, $80, $00, $00, $00, $80, $05, $00, $00, $03,
      $00, $00, $02, $80, $00, $00, $55, $80, $00, $00, $55, $80, $05, $00, $00, $03, $00, $00, $04, $80, $00, $00, $55, $B0, $00, $00, $55, $A0, $04, $00, $00, $04, $00, $00, $04, $80, $00, $00, $00, $B0,
      $00, $00, $00, $A0, $00, $00, $AA, $80, $02, $00, $00, $03, $01, $00, $01, $80, $00, $00, $AA, $80, $00, $00, $AA, $A0, $05, $00, $00, $03, $02, $00, $01, $80, $00, $00, $00, $80, $01, $00, $00, $80,
      $05, $00, $00, $03, $00, $00, $04, $80, $00, $00, $55, $B0, $01, $00, $55, $A0, $04, $00, $00, $04, $00, $00, $04, $80, $00, $00, $00, $B0, $01, $00, $00, $A0, $00, $00, $AA, $80, $02, $00, $00, $03,
      $01, $00, $02, $80, $00, $00, $AA, $80, $01, $00, $AA, $A0, $05, $00, $00, $03, $02, $00, $02, $80, $00, $00, $00, $80, $01, $00, $55, $80, $58, $00, $00, $04, $00, $00, $03, $80, $00, $00, $55, $81,
      $01, $00, $E4, $80, $02, $00, $E4, $80, $02, $00, $00, $03, $00, $00, $04, $80, $00, $00, $00, $81, $03, $00, $55, $A0, $58, $00, $00, $04, $00, $00, $04, $80, $00, $00, $AA, $80, $03, $00, $55, $A0,
      $03, $00, $AA, $A0, $58, $00, $00, $04, $00, $00, $08, $80, $00, $00, $00, $80, $03, $00, $55, $A0, $03, $00, $AA, $A0, $05, $00, $00, $03, $00, $00, $04, $80, $00, $00, $AA, $80, $00, $00, $FF, $80,
      $58, $00, $00, $04, $00, $00, $08, $80, $00, $00, $55, $80, $03, $00, $55, $A0, $03, $00, $AA, $A0, $05, $00, $00, $03, $00, $00, $04, $80, $00, $00, $AA, $80, $00, $00, $FF, $80, $02, $00, $00, $03,
      $00, $00, $08, $80, $00, $00, $55, $81, $03, $00, $55, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $58, $00, $00, $04, $00, $00, $01, $80, $00, $00, $FF, $80,
      $03, $00, $55, $A0, $03, $00, $AA, $A0, $05, $00, $00, $03, $00, $00, $01, $80, $00, $00, $AA, $80, $00, $00, $00, $80, $58, $00, $00, $04, $00, $00, $0F, $80, $00, $00, $00, $81, $03, $00, $AA, $A0,
      $01, $00, $E4, $80, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Matrix1', TContextShaderVariableKind.Vector, 0, 1),
      TContextShaderVariable.Create('Matrix2', TContextShaderVariableKind.Vector, 1, 1),
      TContextShaderVariable.Create('Matrix3', TContextShaderVariableKind.Vector, 2, 1)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX11_level_9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $8B, $C9, $A3, $ED, $2B, $C4, $0E, $6E, $F4, $3B, $5D, $13, $8C, $4B, $90, $E7, $01, $00, $00, $00, $54, $07, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $48, $02, $00, $00,
      $D0, $04, $00, $00, $4C, $05, $00, $00, $EC, $06, $00, $00, $20, $07, $00, $00, $41, $6F, $6E, $39, $08, $02, $00, $00, $08, $02, $00, $00, $00, $02, $FF, $FF, $D4, $01, $00, $00, $34, $00, $00, $00,
      $01, $00, $28, $00, $00, $00, $34, $00, $00, $00, $34, $00, $01, $00, $24, $00, $00, $00, $34, $00, $00, $00, $00, $00, $00, $00, $01, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $02, $FF, $FF,
      $51, $00, $00, $05, $03, $00, $0F, $A0, $00, $00, $80, $BF, $00, $00, $80, $3F, $00, $00, $00, $80, $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02,
      $00, $00, $00, $90, $00, $08, $0F, $A0, $05, $00, $00, $03, $00, $00, $08, $80, $00, $00, $55, $B0, $02, $00, $55, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $00, $A0, $00, $00, $00, $B0,
      $00, $00, $FF, $80, $02, $00, $00, $03, $00, $00, $01, $80, $00, $00, $00, $80, $02, $00, $AA, $A0, $02, $00, $00, $03, $00, $00, $02, $80, $00, $00, $00, $80, $03, $00, $00, $A0, $06, $00, $00, $02,
      $00, $00, $01, $80, $00, $00, $00, $80, $05, $00, $00, $03, $00, $00, $02, $80, $00, $00, $55, $80, $00, $00, $55, $80, $05, $00, $00, $03, $00, $00, $04, $80, $00, $00, $55, $B0, $00, $00, $55, $A0,
      $04, $00, $00, $04, $00, $00, $04, $80, $00, $00, $00, $B0, $00, $00, $00, $A0, $00, $00, $AA, $80, $02, $00, $00, $03, $01, $00, $01, $80, $00, $00, $AA, $80, $00, $00, $AA, $A0, $05, $00, $00, $03,
      $02, $00, $01, $80, $00, $00, $00, $80, $01, $00, $00, $80, $05, $00, $00, $03, $00, $00, $04, $80, $00, $00, $55, $B0, $01, $00, $55, $A0, $04, $00, $00, $04, $00, $00, $04, $80, $00, $00, $00, $B0,
      $01, $00, $00, $A0, $00, $00, $AA, $80, $02, $00, $00, $03, $01, $00, $02, $80, $00, $00, $AA, $80, $01, $00, $AA, $A0, $05, $00, $00, $03, $02, $00, $02, $80, $00, $00, $00, $80, $01, $00, $55, $80,
      $58, $00, $00, $04, $00, $00, $03, $80, $00, $00, $55, $81, $01, $00, $E4, $80, $02, $00, $E4, $80, $02, $00, $00, $03, $00, $00, $04, $80, $00, $00, $55, $81, $03, $00, $55, $A0, $58, $00, $00, $04,
      $00, $00, $04, $80, $00, $00, $AA, $80, $03, $00, $00, $A0, $03, $00, $AA, $A0, $58, $00, $00, $04, $00, $00, $04, $80, $00, $00, $55, $80, $00, $00, $AA, $80, $03, $00, $AA, $A0, $02, $00, $00, $03,
      $00, $00, $08, $80, $00, $00, $00, $81, $03, $00, $55, $A0, $58, $00, $00, $04, $00, $00, $04, $80, $00, $00, $FF, $80, $00, $00, $AA, $80, $03, $00, $AA, $A0, $58, $00, $00, $04, $00, $00, $04, $80,
      $00, $00, $00, $80, $00, $00, $AA, $80, $03, $00, $AA, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $58, $00, $00, $04, $00, $00, $0F, $80, $00, $00, $AA, $80,
      $03, $00, $FF, $A0, $01, $00, $E4, $80, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00, $53, $48, $44, $52, $80, $02, $00, $00, $40, $00, $00, $00, $A0, $00, $00, $00,
      $59, $00, $00, $04, $46, $8E, $20, $00, $00, $00, $00, $00, $04, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $00, $00, $00, $00, $58, $18, $00, $04, $00, $70, $10, $00, $00, $00, $00, $00,
      $55, $55, $00, $00, $62, $10, $00, $03, $32, $10, $10, $00, $00, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00, $00, $00, $00, $00, $68, $00, $00, $02, $03, $00, $00, $00, $0F, $00, $00, $08,
      $12, $00, $10, $00, $00, $00, $00, $00, $46, $80, $20, $00, $00, $00, $00, $00, $03, $00, $00, $00, $46, $10, $10, $00, $00, $00, $00, $00, $00, $00, $00, $08, $12, $00, $10, $00, $00, $00, $00, $00,
      $0A, $00, $10, $00, $00, $00, $00, $00, $2A, $80, $20, $00, $00, $00, $00, $00, $03, $00, $00, $00, $0E, $00, $00, $0A, $22, $00, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00, $00, $00, $80, $3F,
      $00, $00, $80, $3F, $00, $00, $80, $3F, $00, $00, $80, $3F, $0A, $00, $10, $00, $00, $00, $00, $00, $18, $00, $00, $07, $12, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00, $00, $00, $00, $00,
      $01, $40, $00, $00, $00, $00, $80, $3F, $0F, $00, $00, $08, $42, $00, $10, $00, $00, $00, $00, $00, $46, $10, $10, $00, $00, $00, $00, $00, $46, $80, $20, $00, $00, $00, $00, $00, $01, $00, $00, $00,
      $00, $00, $00, $08, $12, $00, $10, $00, $01, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $2A, $80, $20, $00, $00, $00, $00, $00, $01, $00, $00, $00, $0F, $00, $00, $08, $42, $00, $10, $00,
      $00, $00, $00, $00, $46, $10, $10, $00, $00, $00, $00, $00, $46, $80, $20, $00, $00, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $08, $22, $00, $10, $00, $01, $00, $00, $00, $2A, $00, $10, $00,
      $00, $00, $00, $00, $2A, $80, $20, $00, $00, $00, $00, $00, $02, $00, $00, $00, $38, $00, $00, $07, $62, $00, $10, $00, $00, $00, $00, $00, $56, $05, $10, $00, $00, $00, $00, $00, $06, $01, $10, $00,
      $01, $00, $00, $00, $37, $00, $00, $09, $32, $00, $10, $00, $00, $00, $00, $00, $06, $00, $10, $00, $00, $00, $00, $00, $46, $00, $10, $00, $01, $00, $00, $00, $96, $05, $10, $00, $00, $00, $00, $00,
      $1D, $00, $00, $0A, $C2, $00, $10, $00, $00, $00, $00, $00, $06, $04, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $1D, $00, $00, $0A, $32, $00, $10, $00, $01, $00, $00, $00, $02, $40, $00, $00, $00, $00, $80, $3F, $00, $00, $80, $3F, $00, $00, $00, $00, $00, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00,
      $45, $00, $00, $09, $F2, $00, $10, $00, $02, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00, $00, $00, $00, $00, $00, $60, $10, $00, $00, $00, $00, $00, $01, $00, $00, $07,
      $12, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00, $01, $00, $00, $00, $01, $00, $00, $07, $12, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00,
      $00, $00, $00, $00, $0A, $00, $10, $00, $00, $00, $00, $00, $01, $00, $00, $07, $12, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00, $01, $00, $00, $00, $0A, $00, $10, $00, $00, $00, $00, $00,
      $01, $00, $00, $07, $F2, $20, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $02, $00, $00, $00, $06, $00, $10, $00, $00, $00, $00, $00, $3E, $00, $00, $01, $53, $54, $41, $54, $74, $00, $00, $00,
      $12, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $02, $00, $00, $00, $0B, $00, $00, $00, $00, $00, $00, $00, $04, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $52, $44, $45, $46,
      $98, $01, $00, $00, $01, $00, $00, $00, $8C, $00, $00, $00, $03, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FF, $FF, $00, $11, $00, $00, $64, $01, $00, $00, $7C, $00, $00, $00, $03, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $7C, $00, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00,
      $FF, $FF, $FF, $FF, $00, $00, $00, $00, $01, $00, $00, $00, $0C, $00, $00, $00, $82, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $49, $6E, $70, $75, $74, $00, $24, $47, $6C, $6F, $62, $61, $6C, $73, $00, $AB, $82, $00, $00, $00, $05, $00, $00, $00, $A4, $00, $00, $00, $40, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $1C, $01, $00, $00, $00, $00, $00, $00, $04, $00, $00, $00, $00, $00, $00, $00, $24, $01, $00, $00, $00, $00, $00, $00, $34, $01, $00, $00, $04, $00, $00, $00,
      $04, $00, $00, $00, $00, $00, $00, $00, $24, $01, $00, $00, $00, $00, $00, $00, $3B, $01, $00, $00, $10, $00, $00, $00, $0C, $00, $00, $00, $02, $00, $00, $00, $44, $01, $00, $00, $00, $00, $00, $00,
      $54, $01, $00, $00, $20, $00, $00, $00, $0C, $00, $00, $00, $02, $00, $00, $00, $44, $01, $00, $00, $00, $00, $00, $00, $5C, $01, $00, $00, $30, $00, $00, $00, $0C, $00, $00, $00, $02, $00, $00, $00,
      $44, $01, $00, $00, $00, $00, $00, $00, $57, $69, $64, $74, $68, $00, $AB, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $48, $65, $69, $67, $68, $74, $00, $4D,
      $61, $74, $72, $69, $78, $31, $00, $AB, $01, $00, $03, $00, $01, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $4D, $61, $74, $72, $69, $78, $32, $00, $4D, $61, $74, $72, $69, $78, $33, $00,
      $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E,
      $39, $35, $32, $2E, $32, $38, $34, $34, $00, $AB, $AB, $AB, $49, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $03, $00, $00, $00, $00, $00, $00, $00, $03, $03, $00, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $AB, $AB, $AB, $4F, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00,
      $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $53, $56, $5F, $54, $61, $72, $67, $65, $74, $00, $AB, $AB], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 4),
      TContextShaderVariable.Create('Height', TContextShaderVariableKind.Float, 4, 4),
      TContextShaderVariable.Create('Matrix1', TContextShaderVariableKind.Vector, 16, 12),
      TContextShaderVariable.Create('Matrix2', TContextShaderVariableKind.Vector, 32, 12),
      TContextShaderVariable.Create('Matrix3', TContextShaderVariableKind.Vector, 48, 12)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.Metal'}
    {$IF defined(MACOS)}
    TContextShaderSource.Create(
      TContextShaderArch.Metal,
      TEncoding.UTF8.GetBytes(
        'using namespace metal;'+

        'struct ProjectedVertex {'+
          'float4 position [[position]];'+
          'float2 textureCoord;'+
        '};'+

        'float2 applymatrix(float2 val, float3x2 mat, float4 Matrix3)'+
        '{'+
          'float2 newval;'+
          'float z = Matrix3.x * val.x + Matrix3.y * val.y + Matrix3.z;'+
          'if (z == 1) {'+
            'newval.x = val.x * mat[0][0] + val.y * mat[1][0] + mat[2][0];'+
            'newval.y = val.x * mat[0][1] + val.y * mat[1][1] + mat[2][1];'+
          '} else {'+
            'z = 1 / z;'+
            'newval.x = (val.x * mat[0][0] + val.y * mat[1][0] + mat[2][0]) * z;'+
            'newval.y = (val.x * mat[0][1] + val.y * mat[1][1] + mat[2][1]) * z;'+
          '}'+
          'return newval;'+
        '}'+

        'fragment float4 fragmentShader(const ProjectedVertex in [[stage_in]],'+
                                       'constant float4 &Matrix1 [[buffer(0)]],'+
                                       'constant float4 &Matrix2 [[buffer(1)]],'+
                                       'constant float4 &Matrix3 [[buffer(2)]],'+
                                       'const texture2d<float> Input [[texture(0)]],'+
                                       'const sampler InputSampler [[sampler(0)]]) {'+
          'float3x2 Matrix;'+
          'Matrix[0][0] = Matrix1[0];'+
          'Matrix[1][0] = Matrix1[1];'+
          'Matrix[2][0] = Matrix1[2];'+
          'Matrix[0][1] = Matrix2[0];'+
          'Matrix[1][1] = Matrix2[1];'+
          'Matrix[2][1] = Matrix2[2];'+
          'float2 newCoord = in.textureCoord;'+
          'newCoord = applymatrix(newCoord, Matrix, Matrix3);'+
          'bool isValid = (newCoord.x >= 0 && newCoord.x <= 1 && newCoord.y >= 0 && newCoord.y <= 1);'+
          'return isValid ? Input.sample(InputSampler, newCoord) : float4(0, 0, 0, 0);'+
        '}'
      ),
      [TContextShaderVariable.Create('Matrix1', TContextShaderVariableKind.Float3, 0, 1),
       TContextShaderVariable.Create('Matrix2', TContextShaderVariableKind.Float3, 1, 1),
       TContextShaderVariable.Create('Matrix3', TContextShaderVariableKind.Float3, 2, 1),
       TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.GLSL'}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0A, $0D, $2F, $2F, $20, $67, $6C, $73, $6C, $66, $20, $6F, $75, $74, $70, $75, $74, $20, $62, $79, $20, $43,
      $67, $20, $63, $6F, $6D, $70, $69, $6C, $65, $72, $0A, $2F, $2F, $20, $63, $67, $63, $20, $76, $65, $72, $73, $69, $6F, $6E, $20, $33, $2E, $30, $2E, $30, $30, $31, $36, $2C, $20, $62, $75, $69, $6C,
      $64, $20, $64, $61, $74, $65, $20, $46, $65, $62, $20, $31, $31, $20, $32, $30, $31, $31, $0A, $2F, $2F, $20, $63, $6F, $6D, $6D, $61, $6E, $64, $20, $6C, $69, $6E, $65, $20, $61, $72, $67, $73, $3A,
      $20, $2D, $71, $20, $2D, $70, $72, $6F, $66, $69, $6C, $65, $20, $67, $6C, $73, $6C, $66, $20, $2D, $65, $6E, $74, $72, $79, $20, $6D, $61, $69, $6E, $0A, $2F, $2F, $20, $73, $6F, $75, $72, $63, $65,
      $20, $66, $69, $6C, $65, $3A, $20, $70, $72, $6F, $6A, $65, $63, $74, $69, $76, $65, $2E, $70, $73, $0A, $2F, $2F, $76, $65, $6E, $64, $6F, $72, $20, $4E, $56, $49, $44, $49, $41, $20, $43, $6F, $72,
      $70, $6F, $72, $61, $74, $69, $6F, $6E, $0A, $2F, $2F, $76, $65, $72, $73, $69, $6F, $6E, $20, $33, $2E, $30, $2E, $30, $2E, $31, $36, $0A, $2F, $2F, $70, $72, $6F, $66, $69, $6C, $65, $20, $67, $6C,
      $73, $6C, $66, $0A, $2F, $2F, $70, $72, $6F, $67, $72, $61, $6D, $20, $6D, $61, $69, $6E, $0A, $2F, $2F, $73, $65, $6D, $61, $6E, $74, $69, $63, $20, $49, $6E, $70, $75, $74, $0A, $2F, $2F, $73, $65,
      $6D, $61, $6E, $74, $69, $63, $20, $57, $69, $64, $74, $68, $0A, $2F, $2F, $73, $65, $6D, $61, $6E, $74, $69, $63, $20, $48, $65, $69, $67, $68, $74, $0A, $2F, $2F, $73, $65, $6D, $61, $6E, $74, $69,
      $63, $20, $4D, $61, $74, $72, $69, $78, $31, $0A, $2F, $2F, $73, $65, $6D, $61, $6E, $74, $69, $63, $20, $4D, $61, $74, $72, $69, $78, $32, $0A, $2F, $2F, $73, $65, $6D, $61, $6E, $74, $69, $63, $20,
      $4D, $61, $74, $72, $69, $78, $33, $0A, $2F, $2F, $76, $61, $72, $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $49, $6E, $70, $75, $74, $20, $3A, $20, $20, $3A, $20, $5F, $49, $6E, $70, $75,
      $74, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20, $66, $6C, $6F, $61, $74, $33, $20, $4D, $61, $74, $72, $69, $78, $31, $20, $3A, $20, $20, $3A, $20, $5F, $4D, $61,
      $74, $72, $69, $78, $31, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20, $66, $6C, $6F, $61, $74, $33, $20, $4D, $61, $74, $72, $69, $78, $32, $20, $3A, $20, $20, $3A,
      $20, $5F, $4D, $61, $74, $72, $69, $78, $32, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20, $66, $6C, $6F, $61, $74, $33, $20, $4D, $61, $74, $72, $69, $78, $33, $20,
      $3A, $20, $20, $3A, $20, $5F, $4D, $61, $74, $72, $69, $78, $33, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20, $66, $6C, $6F, $61, $74, $32, $20, $74, $65, $78, $43,
      $6F, $6F, $72, $64, $20, $3A, $20, $24, $76, $69, $6E, $2E, $54, $45, $58, $43, $4F, $4F, $52, $44, $30, $20, $3A, $20, $54, $45, $58, $30, $20, $3A, $20, $30, $20, $3A, $20, $31, $0A, $2F, $2F, $76,
      $61, $72, $20, $66, $6C, $6F, $61, $74, $34, $20, $6D, $61, $69, $6E, $20, $3A, $20, $24, $76, $6F, $75, $74, $2E, $43, $4F, $4C, $4F, $52, $20, $3A, $20, $43, $4F, $4C, $20, $3A, $20, $2D, $31, $20,
      $3A, $20, $31, $0A, $0A, $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $30, $3B, $0A, $66, $6C, $6F, $61, $74, $20, $5F, $7A, $30, $30, $30, $39, $3B, $0A, $76, $65, $63, $32, $20, $5F, $6E, $65, $77,
      $76, $61, $6C, $30, $30, $30, $39, $3B, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $5F, $49, $6E, $70, $75, $74, $3B, $0A, $75, $6E, $69, $66, $6F,
      $72, $6D, $20, $76, $65, $63, $33, $20, $5F, $4D, $61, $74, $72, $69, $78, $31, $3B, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $33, $20, $5F, $4D, $61, $74, $72, $69, $78, $32, $3B,
      $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $33, $20, $5F, $4D, $61, $74, $72, $69, $78, $33, $3B, $0A, $0A, $20, $2F, $2F, $20, $6D, $61, $69, $6E, $20, $70, $72, $6F, $63, $65, $64,
      $75, $72, $65, $2C, $20, $74, $68, $65, $20, $6F, $72, $69, $67, $69, $6E, $61, $6C, $20, $6E, $61, $6D, $65, $20, $77, $61, $73, $20, $6D, $61, $69, $6E, $0A, $76, $6F, $69, $64, $20, $6D, $61, $69,
      $6E, $28, $29, $0A, $7B, $0A, $0A, $20, $20, $20, $20, $62, $6F, $6F, $6C, $20, $5F, $69, $73, $56, $61, $6C, $69, $64, $3B, $0A, $0A, $20, $20, $20, $20, $5F, $7A, $30, $30, $30, $39, $20, $3D, $20,
      $5F, $4D, $61, $74, $72, $69, $78, $33, $2E, $78, $2A, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $5F, $4D, $61, $74, $72, $69, $78, $33, $2E, $79, $2A, $54, $45, $58, $30, $2E, $79, $20, $2B, $20,
      $5F, $4D, $61, $74, $72, $69, $78, $33, $2E, $7A, $3B, $0A, $20, $20, $20, $20, $69, $66, $20, $28, $5F, $7A, $30, $30, $30, $39, $20, $3D, $3D, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30,
      $45, $2B, $30, $30, $30, $29, $20, $7B, $20, $2F, $2F, $20, $69, $66, $20, $62, $65, $67, $69, $6E, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $6E, $65, $77, $76, $61, $6C, $30, $30, $30, $39,
      $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $2A, $5F, $4D, $61, $74, $72, $69, $78, $31, $2E, $78, $20, $2B, $20, $54, $45, $58, $30, $2E, $79, $2A, $5F, $4D, $61, $74, $72, $69, $78, $31,
      $2E, $79, $20, $2B, $20, $5F, $4D, $61, $74, $72, $69, $78, $31, $2E, $7A, $3B, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $6E, $65, $77, $76, $61, $6C, $30, $30, $30, $39, $2E, $79, $20, $3D,
      $20, $54, $45, $58, $30, $2E, $78, $2A, $5F, $4D, $61, $74, $72, $69, $78, $32, $2E, $78, $20, $2B, $20, $54, $45, $58, $30, $2E, $79, $2A, $5F, $4D, $61, $74, $72, $69, $78, $32, $2E, $79, $20, $2B,
      $20, $5F, $4D, $61, $74, $72, $69, $78, $32, $2E, $7A, $3B, $0A, $20, $20, $20, $20, $7D, $20, $65, $6C, $73, $65, $20, $7B, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $7A, $30, $30, $30, $39,
      $20, $3D, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $7A, $30, $30, $30, $39, $3B, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $6E, $65, $77, $76,
      $61, $6C, $30, $30, $30, $39, $2E, $78, $20, $3D, $20, $28, $54, $45, $58, $30, $2E, $78, $2A, $5F, $4D, $61, $74, $72, $69, $78, $31, $2E, $78, $20, $2B, $20, $54, $45, $58, $30, $2E, $79, $2A, $5F,
      $4D, $61, $74, $72, $69, $78, $31, $2E, $79, $20, $2B, $20, $5F, $4D, $61, $74, $72, $69, $78, $31, $2E, $7A, $29, $2A, $5F, $7A, $30, $30, $30, $39, $3B, $0A, $20, $20, $20, $20, $20, $20, $20, $20,
      $5F, $6E, $65, $77, $76, $61, $6C, $30, $30, $30, $39, $2E, $79, $20, $3D, $20, $28, $54, $45, $58, $30, $2E, $78, $2A, $5F, $4D, $61, $74, $72, $69, $78, $32, $2E, $78, $20, $2B, $20, $54, $45, $58,
      $30, $2E, $79, $2A, $5F, $4D, $61, $74, $72, $69, $78, $32, $2E, $79, $20, $2B, $20, $5F, $4D, $61, $74, $72, $69, $78, $32, $2E, $7A, $29, $2A, $5F, $7A, $30, $30, $30, $39, $3B, $0A, $20, $20, $20,
      $20, $7D, $20, $2F, $2F, $20, $65, $6E, $64, $20, $69, $66, $0A, $20, $20, $20, $20, $5F, $69, $73, $56, $61, $6C, $69, $64, $20, $3D, $20, $5F, $6E, $65, $77, $76, $61, $6C, $30, $30, $30, $39, $2E,
      $78, $20, $3E, $3D, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $20, $26, $26, $20, $5F, $6E, $65, $77, $76, $61, $6C, $30, $30, $30, $39, $2E, $78, $20, $3C, $3D,
      $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $20, $26, $26, $20, $5F, $6E, $65, $77, $76, $61, $6C, $30, $30, $30, $39, $2E, $79, $20, $3E, $3D, $20, $30, $2E, $30,
      $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $20, $26, $26, $20, $5F, $6E, $65, $77, $76, $61, $6C, $30, $30, $30, $39, $2E, $79, $20, $3C, $3D, $20, $31, $2E, $30, $30, $30, $30, $30,
      $30, $30, $30, $45, $2B, $30, $30, $30, $3B, $0A, $20, $20, $20, $20, $69, $66, $20, $28, $5F, $69, $73, $56, $61, $6C, $69, $64, $29, $20, $7B, $20, $2F, $2F, $20, $69, $66, $20, $62, $65, $67, $69,
      $6E, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $30, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $6E, $65, $77,
      $76, $61, $6C, $30, $30, $30, $39, $29, $3B, $0A, $20, $20, $20, $20, $7D, $20, $65, $6C, $73, $65, $20, $7B, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $30, $20, $3D, $20, $76,
      $65, $63, $34, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30,
      $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $3B, $0A, $20, $20, $20, $20, $7D, $20,
      $2F, $2F, $20, $65, $6E, $64, $20, $69, $66, $0A, $20, $20, $20, $20, $67, $6C, $5F, $46, $72, $61, $67, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $54, $4D, $50, $30, $3B, $0A, $20, $20, $20, $20,
      $72, $65, $74, $75, $72, $6E, $3B, $0A, $7D, $20, $2F, $2F, $20, $6D, $61, $69, $6E, $20, $65, $6E, $64, $0A], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Matrix1', TContextShaderVariableKind.Float3, 0, 1),
      TContextShaderVariable.Create('Matrix2', TContextShaderVariableKind.Float3, 0, 1),
      TContextShaderVariable.Create('Matrix3', TContextShaderVariableKind.Float3, 0, 1)]
    )
    {$ENDREGION}

  ]);
end;

procedure TPerspectiveFilter.CalcSize(var W, H: Integer);
var
  P: TPointF;
  W1, H1, WW, HH: Single;
  S: TAffineMatrix;
begin
  CalcMatrix(InputSize.Width, InputSize.Height);
  W1 := -100;
  H1 := -100;
  WW := 100;
  HH := 100;
  P.x := 1; P.y := 1;
  P := AffinePointTransform(P, FMatrix);
  if P.x > W1 then W1 := P.x;
  if P.y > H1 then H1 := P.y;
  if P.x < WW then WW := P.x;
  if P.y < HH then HH := P.y;
  P.x := 0; P.y := 1;
  P := AffinePointTransform(P, FMatrix);
  if P.x > W1 then W1 := P.x;
  if P.y > H1 then H1 := P.y;
  if P.x < WW then WW := P.x;
  if P.y < HH then HH := P.y;
  P.x := 0; P.y := 0;
  P := AffinePointTransform(P, FMatrix);
  if P.x > W1 then W1 := P.x;
  if P.y > H1 then H1 := P.y;
  if P.x < WW then WW := P.x;
  if P.y < HH then HH := P.y;
  P.x := 1; P.y := 0;
  P := AffinePointTransform(P, FMatrix);
  if P.x > W1 then W1 := P.x;
  if P.y > H1 then H1 := P.y;
  if P.x < WW then WW := P.x;
  if P.y < HH then HH := P.y;
  W := InputSize.Width;
  H := InputSize.Height;
  W := round((W1 - WW) * InputSize.Width);
  H := round((H1 - HH) * InputSize.Height);
  // Recalc matrix
  S := IdentityMatrix;
  if W = 0 then
    S.m11 := 0
  else
    S.m11 := InputSize.Width / W;
  if H = 0 then
    S.m22 := 0
  else
    S.m22 := InputSize.Height / H;
  S.m31 := -WW * S.m11;
  S.m32 := -HH * S.m22;
  FMatrix := AffineMatrixMultiply(FMatrix, S);
  FInvMatrix := FMatrix;
  AffineInvertMatrix(FInvMatrix);
end;

procedure TPerspectiveFilter.CalcMatrix(W, H: Integer);
var
  Wx0, Wy0, Wx1, Wy1, Wx2, Wy2, Wx3, Wy3: Single;
  dx1, dx2, px, dy1, dy2, py: Single;
  g, hh, k: Single;
  P: TPointF;
begin
  P := ValuesAsPoint['TopLeft'];
  Wx0 := P.X / W;
  Wy0 := P.Y / H;

  P := ValuesAsPoint['TopRight'];
  Wx1 := P.X / W;
  Wy1 := P.Y / H;

  P := ValuesAsPoint['BottomRight'];
  Wx2 := P.X / W;
  Wy2 := P.Y / H;

  P := ValuesAsPoint['BottomLeft'];
  Wx3 := P.X / W;
  Wy3 := P.Y / H;

  px  := Wx0 - Wx1 + Wx2 - Wx3;
  py  := Wy0 - Wy1 + Wy2 - Wy3;
  dx1 := Wx1 - Wx2;
  dx2 := Wx3 - Wx2;
  dy1 := Wy1 - Wy2;
  dy2 := Wy3 - Wy2;
  k := dx1 * dy2 - dx2 * dy1;
  if k <> 0 then
  begin
    g := (px * dy2 - py * dx2) / k;
    hh := (dx1 * py - dy1 * px) / k;

    FMatrix.m11 := Wx1 - Wx0 + g * Wx1;
    FMatrix.m21 := Wx3 - Wx0 + hh * Wx3;
    FMatrix.m31 := Wx0;
    FMatrix.m12 := Wy1 - Wy0 + g * Wy1;
    FMatrix.m22 := Wy3 - Wy0 + hh * Wy3;
    FMatrix.m32 := Wy0;
    FMatrix.m13 := g;
    FMatrix.m23 := hh;
    FMatrix.m33 := 1;
  end
  else
    FillChar(FMatrix, SizeOf(FMatrix), 0);
end;

procedure TPerspectiveFilter.LoadShaders;
begin
  FilterContext.SetShaders(FVertexShader, FShaders[FPass]);
  FilterContext.SetShaderVariable('Matrix1', [Vector3D(FInvMatrix.m11, FInvMatrix.m21, FInvMatrix.m31, 0)]);
  FilterContext.SetShaderVariable('Matrix2', [Vector3D(FInvMatrix.m12, FInvMatrix.m22, FInvMatrix.m32, 0)]);
  FilterContext.SetShaderVariable('Matrix3', [Vector3D(FInvMatrix.m13, FInvMatrix.m23, FInvMatrix.m33, 0)]);
end;

class function TPerspectiveFilter.FilterAttr: TFilterRec;
begin
  Result := TFilterRec.Create('PerspectiveTransform', 'Applies an perspective transform to an image.', [
    TFilterValueRec.Create('TopLeft', 'Top left point of result transformation.', TFilterValueType.Point, TValue.From<TPointF>(TPointF.Zero),
      TValue.From<TPointF>(TPointF.Zero), TValue.From<TPointF>(TPointF.Create(65535, 65535))),
    TFilterValueRec.Create('TopRight', 'Top right point of result transformation.', TFilterValueType.Point, TValue.From<TPointF>(TPointF.Create(300, 0)),
      TValue.From<TPointF>(TPointF.Zero), TValue.From<TPointF>(TPointF.Create(65535, 65535))),
    TFilterValueRec.Create('BottomRight', 'Bottom right point of result transformation.', TFilterValueType.Point, TValue.From<TPointF>(TPointF.Create(350, 300)),
      TValue.From<TPointF>(TPointF.Zero), TValue.From<TPointF>(TPointF.Create(65535, 65535))),
    TFilterValueRec.Create('BottomLeft', 'Bottom left point of result transformation.', TFilterValueType.Point, TValue.From<TPointF>(TPointF.Create(0, 300)),
      TValue.From<TPointF>(TPointF.Zero), TValue.From<TPointF>(TPointF.Create(65535, 65535)))
  ]);
end;

{ TCropFilter }

constructor TCropFilter.Create;
begin
  inherited;
  FShaders[FPassCount - 1] := TShaderManager.RegisterShaderFromData('crop.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.SKSL'}
    TContextShaderSource.Create(TContextShaderArch.SKSL, [
      $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $68, $61, $64, $65, $72, 
      $20, $49, $6E, $70, $75, $74, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, 
      $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $4C, $65, $66, $74, $54, $6F, $70, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $52, $69, $67, $68, 
      $74, $42, $6F, $74, $74, $6F, $6D, $3B, $68, $61, $6C, $66, $34, $20, $6D, $61, $69, $6E, $28, $66, $6C, $6F, $61, $74, $32, $20, $61, $29, $7B, $66, $6C, $6F, $61, $74, $32, $20, $62, $3D, $61, $2F, 
      $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $66, $6C, $6F, $61, $74, $32, $20, $63, $3D, $52, $69, $67, $68, $74, $42, $6F, $74, $74, $6F, $6D, $2D, $4C, $65, $66, $74, $54, $6F, $70, $3B, 
      $62, $2A, $3D, $63, $3B, $62, $2B, $3D, $4C, $65, $66, $74, $54, $6F, $70, $3B, $62, $6F, $6F, $6C, $20, $64, $3D, $28, $28, $62, $2E, $78, $3E, $3D, $30, $2E, $26, $26, $62, $2E, $78, $3C, $3D, $31, 
      $2E, $29, $26, $26, $62, $2E, $79, $3E, $3D, $30, $2E, $29, $26, $26, $62, $2E, $79, $3C, $3D, $31, $2E, $3B, $72, $65, $74, $75, $72, $6E, $20, $68, $61, $6C, $66, $34, $28, $64, $3F, $66, $6C, $6F, 
      $61, $74, $34, $28, $49, $6E, $70, $75, $74, $2E, $65, $76, $61, $6C, $28, $62, $2A, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $29, $29, $3A, $66, $6C, $6F, $61, $74, 
      $34, $28, $30, $2E, $29, $29, $3B, $7D, $0A, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Resolution', TContextShaderVariableKind.Float2, 0, 8),
      TContextShaderVariable.Create('InputResolution', TContextShaderVariableKind.Float2, 1, 8),
      TContextShaderVariable.Create('LeftTop', TContextShaderVariableKind.Float2, 2, 8),
      TContextShaderVariable.Create('RightBottom', TContextShaderVariableKind.Float2, 3, 8)]
    ),
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FF, $FF, $FE, $FF, $31, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $9B, $00, $00, $00, $00, $02, $FF, $FF, $03, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $94, $00, $00, $00,
      $58, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $60, $00, $00, $00, $00, $00, $00, $00, $70, $00, $00, $00, $02, $00, $00, $00, $01, $00, $00, $00, $78, $00, $00, $00, $00, $00, $00, $00,
      $88, $00, $00, $00, $02, $00, $01, $00, $01, $00, $00, $00, $78, $00, $00, $00, $00, $00, $00, $00, $49, $6E, $70, $75, $74, $00, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00,
      $00, $00, $00, $00, $4C, $65, $66, $74, $54, $6F, $70, $00, $01, $00, $03, $00, $01, $00, $02, $00, $01, $00, $00, $00, $00, $00, $00, $00, $52, $69, $67, $68, $74, $42, $6F, $74, $74, $6F, $6D, $00,
      $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C,
      $65, $72, $20, $00, $51, $00, $00, $05, $02, $00, $0F, $A0, $00, $00, $80, $3F, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0,
      $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $01, $00, $00, $02, $00, $00, $03, $80, $00, $00, $E4, $A0, $02, $00, $00, $03, $00, $00, $03, $80, $00, $00, $E4, $81, $01, $00, $E4, $A0,
      $04, $00, $00, $04, $00, $00, $03, $80, $00, $00, $E4, $B0, $00, $00, $E4, $80, $00, $00, $E4, $A0, $58, $00, $00, $04, $00, $00, $04, $80, $00, $00, $00, $80, $02, $00, $00, $A0, $02, $00, $55, $A0,
      $58, $00, $00, $04, $00, $00, $08, $80, $00, $00, $55, $80, $02, $00, $00, $A0, $02, $00, $55, $A0, $05, $00, $00, $03, $00, $00, $04, $80, $00, $00, $FF, $80, $00, $00, $AA, $80, $02, $00, $00, $03,
      $00, $00, $08, $80, $00, $00, $00, $81, $02, $00, $00, $A0, $58, $00, $00, $04, $00, $00, $08, $80, $00, $00, $FF, $80, $02, $00, $00, $A0, $02, $00, $55, $A0, $05, $00, $00, $03, $00, $00, $04, $80,
      $00, $00, $AA, $80, $00, $00, $FF, $80, $02, $00, $00, $03, $00, $00, $08, $80, $00, $00, $55, $81, $02, $00, $00, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0,
      $58, $00, $00, $04, $00, $00, $01, $80, $00, $00, $FF, $80, $02, $00, $00, $A0, $02, $00, $55, $A0, $05, $00, $00, $03, $00, $00, $01, $80, $00, $00, $AA, $80, $00, $00, $00, $80, $58, $00, $00, $04,
      $00, $00, $0F, $80, $00, $00, $00, $81, $02, $00, $55, $A0, $01, $00, $E4, $80, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('LeftTop', TContextShaderVariableKind.Vector, 0, 1),
      TContextShaderVariable.Create('RightBottom', TContextShaderVariableKind.Vector, 1, 1)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX11_level_9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $21, $72, $9F, $B0, $40, $8D, $02, $B8, $8B, $2B, $D2, $FF, $2C, $4D, $95, $8F, $01, $00, $00, $00, $20, $05, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $74, $01, $00, $00,
      $08, $03, $00, $00, $84, $03, $00, $00, $B8, $04, $00, $00, $EC, $04, $00, $00, $41, $6F, $6E, $39, $34, $01, $00, $00, $34, $01, $00, $00, $00, $02, $FF, $FF, $00, $01, $00, $00, $34, $00, $00, $00,
      $01, $00, $28, $00, $00, $00, $34, $00, $00, $00, $34, $00, $01, $00, $24, $00, $00, $00, $34, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $02, $FF, $FF,
      $51, $00, $00, $05, $01, $00, $0F, $A0, $00, $00, $80, $3F, $00, $00, $80, $BF, $00, $00, $00, $80, $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02,
      $00, $00, $00, $90, $00, $08, $0F, $A0, $12, $00, $00, $04, $00, $00, $01, $80, $00, $00, $00, $B0, $00, $00, $AA, $A0, $00, $00, $00, $A0, $12, $00, $00, $04, $00, $00, $02, $80, $00, $00, $55, $B0,
      $00, $00, $FF, $A0, $00, $00, $55, $A0, $02, $00, $00, $03, $00, $00, $04, $80, $00, $00, $55, $81, $01, $00, $00, $A0, $58, $00, $00, $04, $00, $00, $04, $80, $00, $00, $AA, $80, $01, $00, $55, $A0,
      $01, $00, $AA, $A0, $02, $00, $00, $03, $00, $00, $08, $80, $00, $00, $00, $81, $01, $00, $00, $A0, $58, $00, $00, $04, $00, $00, $04, $80, $00, $00, $FF, $80, $00, $00, $AA, $80, $01, $00, $AA, $A0,
      $58, $00, $00, $04, $00, $00, $04, $80, $00, $00, $55, $80, $00, $00, $AA, $80, $01, $00, $AA, $A0, $58, $00, $00, $04, $00, $00, $04, $80, $00, $00, $00, $80, $00, $00, $AA, $80, $01, $00, $AA, $A0,
      $42, $00, $00, $03, $01, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $58, $00, $00, $04, $00, $00, $0F, $80, $00, $00, $AA, $80, $01, $00, $FF, $A0, $01, $00, $E4, $80, $01, $00, $00, $02,
      $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00, $53, $48, $44, $52, $8C, $01, $00, $00, $40, $00, $00, $00, $63, $00, $00, $00, $59, $00, $00, $04, $46, $8E, $20, $00, $00, $00, $00, $00,
      $01, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $00, $00, $00, $00, $58, $18, $00, $04, $00, $70, $10, $00, $00, $00, $00, $00, $55, $55, $00, $00, $62, $10, $00, $03, $32, $10, $10, $00,
      $00, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00, $00, $00, $00, $00, $68, $00, $00, $02, $03, $00, $00, $00, $00, $00, $00, $0A, $32, $00, $10, $00, $00, $00, $00, $00, $46, $80, $20, $80,
      $41, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $E6, $8A, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $32, $00, $00, $0A, $32, $00, $10, $00, $00, $00, $00, $00, $46, $10, $10, $00,
      $00, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $46, $80, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $1D, $00, $00, $0A, $C2, $00, $10, $00, $00, $00, $00, $00, $06, $04, $10, $00,
      $00, $00, $00, $00, $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $07, $42, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00,
      $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $1D, $00, $00, $0A, $32, $00, $10, $00, $01, $00, $00, $00, $02, $40, $00, $00, $00, $00, $80, $3F, $00, $00, $80, $3F, $00, $00, $00, $00,
      $00, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $45, $00, $00, $09, $F2, $00, $10, $00, $02, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00, $00, $00, $00, $00,
      $00, $60, $10, $00, $00, $00, $00, $00, $01, $00, $00, $07, $12, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00, $01, $00, $00, $00, $01, $00, $00, $07,
      $12, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00, $01, $00, $00, $00, $0A, $00, $10, $00, $00, $00, $00, $00, $01, $00, $00, $07, $F2, $20, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00,
      $02, $00, $00, $00, $06, $00, $10, $00, $00, $00, $00, $00, $3E, $00, $00, $01, $53, $54, $41, $54, $74, $00, $00, $00, $0A, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $02, $00, $00, $00,
      $03, $00, $00, $00, $00, $00, $00, $00, $04, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $52, $44, $45, $46, $2C, $01, $00, $00, $01, $00, $00, $00, $8C, $00, $00, $00, $03, $00, $00, $00,
      $1C, $00, $00, $00, $00, $04, $FF, $FF, $00, $11, $00, $00, $F8, $00, $00, $00, $7C, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $7C, $00, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $01, $00, $00, $00, $0C, $00, $00, $00,
      $82, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $49, $6E, $70, $75, $74, $00, $24, $47,
      $6C, $6F, $62, $61, $6C, $73, $00, $AB, $82, $00, $00, $00, $02, $00, $00, $00, $A4, $00, $00, $00, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $D4, $00, $00, $00, $00, $00, $00, $00,
      $08, $00, $00, $00, $02, $00, $00, $00, $DC, $00, $00, $00, $00, $00, $00, $00, $EC, $00, $00, $00, $08, $00, $00, $00, $08, $00, $00, $00, $02, $00, $00, $00, $DC, $00, $00, $00, $00, $00, $00, $00,
      $4C, $65, $66, $74, $54, $6F, $70, $00, $01, $00, $03, $00, $01, $00, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00, $52, $69, $67, $68, $74, $42, $6F, $74, $74, $6F, $6D, $00, $4D, $69, $63, $72,
      $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E, $39, $35, $32, $2E,
      $32, $38, $34, $34, $00, $AB, $AB, $AB, $49, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00,
      $00, $00, $00, $00, $03, $03, $00, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $AB, $AB, $AB, $4F, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $53, $56, $5F, $54, $61, $72, $67, $65, $74, $00, $AB, $AB], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('LeftTop', TContextShaderVariableKind.Vector, 0, 8),
      TContextShaderVariable.Create('RightBottom', TContextShaderVariableKind.Vector, 8, 8)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.Metal'}
    {$IF defined(MACOS)}
    TContextShaderSource.Create(
      TContextShaderArch.Metal,
      TEncoding.UTF8.GetBytes(
        'using namespace metal;'+

        'struct ProjectedVertex {'+
          'float4 position [[position]];'+
          'float2 textureCoord;'+
        '};'+

        'fragment float4 fragmentShader(const ProjectedVertex in [[stage_in]],'+
                                       'constant float4 &LeftTop [[buffer(0)]],'+
                                       'constant float4 &RightBottom [[buffer(1)]],'+
                                       'const texture2d<float> Input [[texture(0)]],'+
                                       'const sampler InputSampler [[sampler(0)]]) {'+
          'float2 newSize = RightBottom.xy - LeftTop.xy;'+
          'float2 newCoord = in.textureCoord;'+
          'newCoord *= newSize;'+
          'newCoord += LeftTop.xy;'+
          'bool isValid = (newCoord.x >= 0 && newCoord.y >= 0 && newCoord.x <= 1 && newCoord.y <= 1);'+
          'return isValid ? Input.sample(InputSampler, newCoord) : float4(0, 0, 0, 0);'+
        '}'
      ),
      [TContextShaderVariable.Create('LeftTop', TContextShaderVariableKind.Float2, 0, 1),
       TContextShaderVariable.Create('RightBottom', TContextShaderVariableKind.Float2, 1, 1),
       TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.GLSL'}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $30, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D,
      $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $5F, $49, $6E, $70, $75, $74, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $32, $20, $5F, $4C, $65, $66, $74, $54, $6F,
      $70, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $32, $20, $5F, $52, $69, $67, $68, $74, $42, $6F, $74, $74, $6F, $6D, $3B, $0D, $0A, $76, $6F, $69, $64, $20, $6D, $61, $69,
      $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20, $20, $76, $65, $63, $32, $20, $5F, $6E, $65, $77, $53, $69, $7A, $65, $3B, $0D, $0A, $20, $20, $20, $20, $76, $65, $63, $32, $20, $5F, $6E, $65,
      $77, $43, $6F, $6F, $72, $64, $3B, $0D, $0A, $20, $20, $20, $20, $62, $6F, $6F, $6C, $20, $5F, $69, $73, $56, $61, $6C, $69, $64, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $53, $69, $7A,
      $65, $20, $3D, $20, $5F, $52, $69, $67, $68, $74, $42, $6F, $74, $74, $6F, $6D, $20, $2D, $20, $5F, $4C, $65, $66, $74, $54, $6F, $70, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $20, $3D, $20, $76, $65, $63, $32, $28, $54, $45, $58, $30, $2E, $78, $2C, $20, $31, $2E, $30, $20, $2D, $20, $54, $45, $58, $30, $2E, $79, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F,
      $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2A, $5F, $6E, $65, $77, $53, $69, $7A, $65, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77,
      $43, $6F, $6F, $72, $64, $20, $3D, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $2B, $20, $5F, $4C, $65, $66, $74, $54, $6F, $70, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43,
      $6F, $6F, $72, $64, $20, $3D, $20, $76, $65, $63, $32, $28, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $2C, $20, $31, $2E, $30, $20, $2D, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64,
      $2E, $79, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $69, $73, $56, $61, $6C, $69, $64, $20, $3D, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3E, $3D, $20, $30, $2E, $30, $20,
      $26, $26, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3E, $3D, $20, $30, $2E, $30, $20, $26, $26, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3C, $3D, $20,
      $31, $2E, $30, $20, $26, $26, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3C, $3D, $20, $31, $2E, $30, $3B, $0D, $0A, $20, $20, $20, $20, $69, $66, $20, $28, $5F, $69, $73, $56,
      $61, $6C, $69, $64, $29, $20, $7B, $20, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $30, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70,
      $75, $74, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $7D, $20, $65, $6C, $73, $65, $20, $7B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F,
      $54, $4D, $50, $30, $20, $3D, $20, $76, $65, $63, $34, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $3B, $0D, $0A, $20, $20, $20, $20, $7D,
      $20, $2F, $2F, $20, $65, $6E, $64, $20, $69, $66, $0D, $0A, $20, $20, $20, $20, $67, $6C, $5F, $46, $72, $61, $67, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $54, $4D, $50, $30, $3B, $0D, $0A, $20,
      $20, $20, $20, $72, $65, $74, $75, $72, $6E, $3B, $0D, $0A, $7D, $20, $0D, $0A], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('LeftTop', TContextShaderVariableKind.Float2, 0, 1),
      TContextShaderVariable.Create('RightBottom', TContextShaderVariableKind.Float2, 0, 1)]
    )
    {$ENDREGION}

  ]);
end;

procedure TCropFilter.CalcSize(var W, H: Integer);
begin
  W := round(ValuesAsPoint['RightBottom'].x - ValuesAsPoint['LeftTop'].x);
  H := round(ValuesAsPoint['RightBottom'].y - ValuesAsPoint['LeftTop'].y);
end;

class function TCropFilter.FilterAttr: TFilterRec;
begin
  Result := TFilterRec.Create('Crop', 'The size and shape of the cropped image depend on the rectangle you specify.', [
    TFilterValueRec.Create('LeftTop', 'Left-top corner of cropping rect', TFilterValueType.Point, TValue.From<TPointF>(TPointF.Zero),
      TValue.From<TPointF>(TPointF.Zero), TValue.From<TPointF>(TPointF.Create(65535, 65535))),
    TFilterValueRec.Create('RightBottom', 'Left-top corner of cropping rect', TFilterValueType.Point, TValue.From<TPointF>(TPointF.Create(150, 150)),
      TValue.From<TPointF>(TPointF.Zero), TValue.From<TPointF>(TPointF.Create(65535, 65535)))
  ]);
end;

{ TGaussianBlurFilter }

constructor TGaussianBlurFilter.Create;
begin
  inherited;
  FShaders[0] := TShaderManager.RegisterShaderFromData('gaussianblurh.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.SKSL'}
    TContextShaderSource.Create(TContextShaderArch.SKSL, [
      $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $68, $61, $64, $65, $72, 
      $20, $49, $6E, $70, $75, $74, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, 
      $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $57, $69, $64, $74, $68, $3B, $69, $6E, $6C, $69, $6E, $65, $20, $66, $6C, $6F, $61, $74, $20, $61, $28, $63, $6F, $6E, $73, $74, $20, $69, 
      $6E, $74, $20, $62, $29, $7B, $73, $77, $69, $74, $63, $68, $28, $62, $29, $7B, $63, $61, $73, $65, $20, $30, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $33, $38, $38, $37, $32, $35, $34, $3B, $63, 
      $61, $73, $65, $20, $31, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $34, $33, $31, $37, $33, $34, $38, $3B, $63, $61, $73, $65, $20, $32, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $34, $37, $33, 
      $36, $31, $39, $34, $3B, $63, $61, $73, $65, $20, $33, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $35, $31, $33, $31, $39, $32, $35, $3B, $63, $61, $73, $65, $20, $34, $3A, $72, $65, $74, $75, $72, 
      $6E, $2E, $30, $35, $34, $39, $32, $34, $39, $32, $3B, $63, $61, $73, $65, $20, $35, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $35, $38, $30, $36, $32, $36, $35, $34, $3B, $63, $61, $73, $65, $20, 
      $36, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $36, $30, $36, $32, $36, $35, $33, $3B, $63, $61, $73, $65, $20, $37, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $36, $32, $35, $32, $36, $39, $3B, 
      $63, $61, $73, $65, $20, $38, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $36, $33, $36, $39, $35, $35, $39, $3B, $63, $61, $73, $65, $20, $39, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $33, $38, 
      $38, $37, $32, $35, $34, $3B, $63, $61, $73, $65, $20, $31, $30, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $36, $33, $36, $39, $35, $35, $39, $3B, $63, $61, $73, $65, $20, $31, $31, $3A, $72, $65, 
      $74, $75, $72, $6E, $2E, $30, $36, $32, $35, $32, $36, $39, $3B, $63, $61, $73, $65, $20, $31, $32, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $36, $30, $36, $32, $36, $35, $33, $3B, $63, $61, $73, 
      $65, $20, $31, $33, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $35, $38, $30, $36, $32, $36, $35, $34, $3B, $63, $61, $73, $65, $20, $31, $34, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $35, $34, 
      $39, $32, $34, $39, $32, $3B, $63, $61, $73, $65, $20, $31, $35, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $35, $31, $33, $31, $39, $32, $35, $3B, $63, $61, $73, $65, $20, $31, $36, $3A, $72, $65, 
      $74, $75, $72, $6E, $2E, $30, $34, $37, $33, $36, $31, $39, $34, $3B, $63, $61, $73, $65, $20, $31, $37, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $34, $33, $31, $37, $33, $34, $38, $3B, $64, $65, 
      $66, $61, $75, $6C, $74, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $33, $38, $38, $37, $32, $35, $34, $3B, $7D, $7D, $68, $61, $6C, $66, $34, $20, $6D, $61, $69, $6E, $28, $66, $6C, $6F, $61, $74, 
      $32, $20, $62, $29, $7B, $66, $6C, $6F, $61, $74, $32, $20, $63, $3D, $62, $2F, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $66, $6C, $6F, $61, $74, $34, $20, $64, $3D, $66, $6C, $6F, $61, 
      $74, $34, $28, $30, $2E, $29, $3B, $66, $6F, $72, $28, $69, $6E, $74, $20, $65, $3D, $30, $3B, $65, $3C, $31, $39, $3B, $65, $2B, $2B, $29, $7B, $66, $6C, $6F, $61, $74, $32, $20, $66, $3D, $63, $3B, 
      $66, $2E, $78, $2B, $3D, $28, $28, $66, $6C, $6F, $61, $74, $28, $65, $29, $2D, $39, $2E, $35, $29, $2B, $31, $2E, $29, $2F, $57, $69, $64, $74, $68, $3B, $64, $2B, $3D, $66, $6C, $6F, $61, $74, $34, 
      $28, $49, $6E, $70, $75, $74, $2E, $65, $76, $61, $6C, $28, $66, $2A, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $29, $29, $2A, $61, $28, $65, $29, $3B, $7D, $72, $65, 
      $74, $75, $72, $6E, $20, $68, $61, $6C, $66, $34, $28, $64, $29, $3B, $7D, $0A, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Resolution', TContextShaderVariableKind.Float2, 0, 8),
      TContextShaderVariable.Create('InputResolution', TContextShaderVariableKind.Float2, 1, 8),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 2, 4)]
    ),
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FF, $FF, $FE, $FF, $29, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $7B, $00, $00, $00, $00, $02, $FF, $FF, $02, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $74, $00, $00, $00,
      $44, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $4C, $00, $00, $00, $00, $00, $00, $00, $5C, $00, $00, $00, $02, $00, $00, $00, $01, $00, $00, $00, $64, $00, $00, $00, $00, $00, $00, $00,
      $49, $6E, $70, $75, $74, $00, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $57, $69, $64, $74, $68, $00, $AB, $AB, $00, $00, $03, $00, $01, $00, $01, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65,
      $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $51, $00, $00, $05, $01, $00, $0F, $A0, $00, $00, $C0, $C0, $98, $FE, $41, $3D, $00, $00, $A0, $C0, $22, $34, $52, $3D, $51, $00, $00, $05,
      $02, $00, $0F, $A0, $00, $00, $00, $C0, $86, $53, $78, $3D, $1A, $0E, $80, $3D, $D5, $72, $82, $3D, $51, $00, $00, $05, $03, $00, $0F, $A0, $00, $00, $80, $C0, $F4, $F8, $60, $3D, $00, $00, $40, $C0,
      $1B, $D3, $6D, $3D, $51, $00, $00, $05, $04, $00, $0F, $A0, $00, $00, $00, $C1, $00, $00, $E0, $C0, $AD, $D6, $30, $3D, $D0, $38, $1F, $3D, $51, $00, $00, $05, $05, $00, $0F, $A0, $00, $00, $10, $41,
      $00, $00, $20, $41, $00, $00, $00, $00, $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $01, $00, $00, $02,
      $00, $00, $12, $80, $00, $00, $55, $B0, $06, $00, $00, $02, $00, $00, $04, $80, $00, $00, $00, $A0, $04, $00, $00, $04, $00, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $55, $A0, $00, $00, $00, $B0,
      $01, $00, $00, $02, $01, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $01, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $02, $00, $12, $80,
      $00, $00, $55, $B0, $04, $00, $00, $04, $02, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $03, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04,
      $03, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $AA, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $04, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $04, $00, $11, $80, $00, $00, $AA, $80,
      $03, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $05, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $05, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $AA, $A0, $00, $00, $00, $B0,
      $01, $00, $00, $02, $06, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $06, $00, $11, $80, $00, $00, $AA, $80, $02, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $07, $00, $12, $80,
      $00, $00, $55, $B0, $02, $00, $00, $03, $07, $00, $11, $80, $00, $00, $AA, $81, $00, $00, $00, $B0, $01, $00, $00, $02, $08, $00, $13, $80, $00, $00, $E4, $B0, $42, $00, $00, $03, $09, $00, $0F, $80,
      $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80,
      $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $08, $00, $0F, $80, $08, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03, $09, $00, $0F, $80, $09, $00, $E4, $80, $04, $00, $AA, $A0, $04, $00, $00, $04, $01, $00, $0F, $80,
      $01, $00, $E4, $80, $04, $00, $FF, $A0, $09, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $02, $00, $E4, $80, $01, $00, $55, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80,
      $03, $00, $E4, $80, $01, $00, $FF, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $04, $00, $E4, $80, $03, $00, $55, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80,
      $05, $00, $E4, $80, $03, $00, $FF, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $06, $00, $E4, $80, $02, $00, $55, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80,
      $07, $00, $E4, $80, $02, $00, $AA, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $08, $00, $E4, $80, $02, $00, $FF, $A0, $01, $00, $E4, $80, $01, $00, $00, $02, $00, $00, $12, $80,
      $00, $00, $55, $B0, $02, $00, $00, $03, $00, $00, $11, $80, $00, $00, $AA, $80, $00, $00, $00, $B0, $01, $00, $00, $02, $02, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $02, $00, $11, $80,
      $00, $00, $AA, $80, $02, $00, $00, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $03, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $03, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $AA, $A1,
      $00, $00, $00, $B0, $01, $00, $00, $02, $04, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $04, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $00, $A1, $00, $00, $00, $B0, $01, $00, $00, $02,
      $05, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $05, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $AA, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $06, $00, $12, $80, $00, $00, $55, $B0,
      $04, $00, $00, $04, $06, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $00, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $07, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $07, $00, $11, $80,
      $00, $00, $AA, $80, $04, $00, $55, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $08, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $08, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $00, $A1,
      $00, $00, $00, $B0, $42, $00, $00, $03, $09, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
      $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80,
      $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
      $08, $00, $0F, $80, $08, $00, $E4, $80, $00, $08, $E4, $A0, $04, $00, $00, $04, $01, $00, $0F, $80, $09, $00, $E4, $80, $04, $00, $FF, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80,
      $02, $00, $E4, $80, $02, $00, $FF, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $03, $00, $E4, $80, $02, $00, $AA, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80,
      $04, $00, $E4, $80, $02, $00, $55, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $05, $00, $E4, $80, $03, $00, $FF, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80,
      $06, $00, $E4, $80, $03, $00, $55, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $07, $00, $E4, $80, $01, $00, $FF, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80,
      $08, $00, $E4, $80, $01, $00, $55, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $00, $00, $11, $80, $00, $00, $AA, $80, $05, $00, $00, $A0, $00, $00, $00, $B0, $04, $00, $00, $04, $02, $00, $11, $80,
      $00, $00, $AA, $80, $05, $00, $55, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $00, $00, $12, $80, $00, $00, $55, $B0, $01, $00, $00, $02, $02, $00, $12, $80, $00, $00, $55, $B0, $42, $00, $00, $03,
      $00, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $04, $00, $00, $04, $00, $00, $0F, $80, $00, $00, $E4, $80,
      $04, $00, $AA, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $00, $00, $0F, $80, $02, $00, $E4, $80, $04, $00, $FF, $A0, $00, $00, $E4, $80, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80,
      $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX11_level_9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $9A, $60, $E7, $AE, $68, $A4, $A9, $BB, $09, $1B, $65, $7E, $80, $C3, $25, $38, $01, $00, $00, $00, $00, $0B, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $10, $06, $00, $00,
      $0C, $09, $00, $00, $88, $09, $00, $00, $98, $0A, $00, $00, $CC, $0A, $00, $00, $41, $6F, $6E, $39, $D0, $05, $00, $00, $D0, $05, $00, $00, $00, $02, $FF, $FF, $9C, $05, $00, $00, $34, $00, $00, $00,
      $01, $00, $28, $00, $00, $00, $34, $00, $00, $00, $34, $00, $01, $00, $24, $00, $00, $00, $34, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $02, $FF, $FF,
      $51, $00, $00, $05, $01, $00, $0F, $A0, $00, $00, $00, $C1, $00, $00, $E0, $C0, $AD, $D6, $30, $3D, $D0, $38, $1F, $3D, $51, $00, $00, $05, $02, $00, $0F, $A0, $00, $00, $C0, $C0, $98, $FE, $41, $3D,
      $00, $00, $A0, $C0, $22, $34, $52, $3D, $51, $00, $00, $05, $03, $00, $0F, $A0, $00, $00, $80, $C0, $F4, $F8, $60, $3D, $00, $00, $40, $C0, $1B, $D3, $6D, $3D, $51, $00, $00, $05, $04, $00, $0F, $A0,
      $00, $00, $00, $C0, $86, $53, $78, $3D, $1A, $0E, $80, $3D, $D5, $72, $82, $3D, $51, $00, $00, $05, $05, $00, $0F, $A0, $00, $00, $10, $41, $00, $00, $20, $41, $00, $00, $00, $00, $00, $00, $00, $00,
      $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $01, $00, $00, $02, $00, $00, $12, $80, $00, $00, $55, $B0, $06, $00, $00, $02,
      $00, $00, $04, $80, $00, $00, $00, $A0, $04, $00, $00, $04, $00, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $55, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $01, $00, $12, $80, $00, $00, $55, $B0,
      $04, $00, $00, $04, $01, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $02, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $02, $00, $11, $80,
      $00, $00, $AA, $80, $02, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $03, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $03, $00, $11, $80, $00, $00, $AA, $80, $02, $00, $AA, $A0,
      $00, $00, $00, $B0, $01, $00, $00, $02, $04, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $04, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02,
      $05, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $05, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $AA, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $06, $00, $12, $80, $00, $00, $55, $B0,
      $04, $00, $00, $04, $06, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $07, $00, $12, $80, $00, $00, $55, $B0, $02, $00, $00, $03, $07, $00, $11, $80,
      $00, $00, $AA, $81, $00, $00, $00, $B0, $01, $00, $00, $02, $08, $00, $13, $80, $00, $00, $E4, $B0, $42, $00, $00, $03, $09, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
      $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80,
      $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
      $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $08, $00, $0F, $80, $08, $00, $E4, $80,
      $00, $08, $E4, $A0, $05, $00, $00, $03, $09, $00, $0F, $80, $09, $00, $E4, $80, $01, $00, $AA, $A0, $04, $00, $00, $04, $01, $00, $0F, $80, $01, $00, $E4, $80, $01, $00, $FF, $A0, $09, $00, $E4, $80,
      $04, $00, $00, $04, $01, $00, $0F, $80, $02, $00, $E4, $80, $02, $00, $55, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $03, $00, $E4, $80, $02, $00, $FF, $A0, $01, $00, $E4, $80,
      $04, $00, $00, $04, $01, $00, $0F, $80, $04, $00, $E4, $80, $03, $00, $55, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $05, $00, $E4, $80, $03, $00, $FF, $A0, $01, $00, $E4, $80,
      $04, $00, $00, $04, $01, $00, $0F, $80, $06, $00, $E4, $80, $04, $00, $55, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $07, $00, $E4, $80, $04, $00, $AA, $A0, $01, $00, $E4, $80,
      $04, $00, $00, $04, $01, $00, $0F, $80, $08, $00, $E4, $80, $04, $00, $FF, $A0, $01, $00, $E4, $80, $01, $00, $00, $02, $00, $00, $12, $80, $00, $00, $55, $B0, $02, $00, $00, $03, $00, $00, $11, $80,
      $00, $00, $AA, $80, $00, $00, $00, $B0, $01, $00, $00, $02, $02, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $02, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $00, $A1, $00, $00, $00, $B0,
      $01, $00, $00, $02, $03, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $03, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $AA, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $04, $00, $12, $80,
      $00, $00, $55, $B0, $04, $00, $00, $04, $04, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $00, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $05, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04,
      $05, $00, $11, $80, $00, $00, $AA, $80, $02, $00, $AA, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $06, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $06, $00, $11, $80, $00, $00, $AA, $80,
      $02, $00, $00, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $07, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $07, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $55, $A1, $00, $00, $00, $B0,
      $01, $00, $00, $02, $08, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $08, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $00, $A1, $00, $00, $00, $B0, $42, $00, $00, $03, $09, $00, $0F, $80,
      $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80,
      $06, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $08, $00, $0F, $80, $08, $00, $E4, $80, $00, $08, $E4, $A0,
      $04, $00, $00, $04, $01, $00, $0F, $80, $09, $00, $E4, $80, $01, $00, $FF, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $02, $00, $E4, $80, $04, $00, $FF, $A0, $01, $00, $E4, $80,
      $04, $00, $00, $04, $01, $00, $0F, $80, $03, $00, $E4, $80, $04, $00, $AA, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $04, $00, $E4, $80, $04, $00, $55, $A0, $01, $00, $E4, $80,
      $04, $00, $00, $04, $01, $00, $0F, $80, $05, $00, $E4, $80, $03, $00, $FF, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $06, $00, $E4, $80, $03, $00, $55, $A0, $01, $00, $E4, $80,
      $04, $00, $00, $04, $01, $00, $0F, $80, $07, $00, $E4, $80, $02, $00, $FF, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $08, $00, $E4, $80, $02, $00, $55, $A0, $01, $00, $E4, $80,
      $04, $00, $00, $04, $00, $00, $11, $80, $00, $00, $AA, $80, $05, $00, $00, $A0, $00, $00, $00, $B0, $04, $00, $00, $04, $02, $00, $11, $80, $00, $00, $AA, $80, $05, $00, $55, $A0, $00, $00, $00, $B0,
      $01, $00, $00, $02, $00, $00, $12, $80, $00, $00, $55, $B0, $01, $00, $00, $02, $02, $00, $12, $80, $00, $00, $55, $B0, $42, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $04, $00, $00, $04, $00, $00, $0F, $80, $00, $00, $E4, $80, $01, $00, $AA, $A0, $01, $00, $E4, $80, $04, $00, $00, $04,
      $00, $00, $0F, $80, $02, $00, $E4, $80, $01, $00, $FF, $A0, $00, $00, $E4, $80, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00, $53, $48, $44, $52, $F4, $02, $00, $00,
      $40, $00, $00, $00, $BD, $00, $00, $00, $35, $18, $00, $00, $4E, $00, $00, $00, $D0, $38, $1F, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $AD, $D6, $30, $3D, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $98, $FE, $41, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $22, $34, $52, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $F4, $F8, $60, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $1B, $D3, $6D, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $86, $53, $78, $3D, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $1A, $0E, $80, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $D5, $72, $82, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $D0, $38, $1F, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $D5, $72, $82, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $1A, $0E, $80, $3D, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $86, $53, $78, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $1B, $D3, $6D, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $F4, $F8, $60, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $22, $34, $52, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $98, $FE, $41, $3D, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $AD, $D6, $30, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $D0, $38, $1F, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $59, $00, $00, $04, $46, $8E, $20, $00, $00, $00, $00, $00, $01, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $00, $00, $00, $00, $58, $18, $00, $04, $00, $70, $10, $00, $00, $00, $00, $00,
      $55, $55, $00, $00, $62, $10, $00, $03, $32, $10, $10, $00, $00, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00, $00, $00, $00, $00, $68, $00, $00, $02, $04, $00, $00, $00, $36, $20, $00, $05,
      $22, $00, $10, $00, $00, $00, $00, $00, $1A, $10, $10, $00, $00, $00, $00, $00, $36, $00, $00, $08, $F2, $00, $10, $00, $01, $00, $00, $00, $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $36, $00, $00, $05, $42, $00, $10, $00, $00, $00, $00, $00, $01, $40, $00, $00, $00, $00, $00, $00, $30, $00, $00, $01, $21, $00, $00, $07, $82, $00, $10, $00,
      $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $01, $40, $00, $00, $13, $00, $00, $00, $03, $00, $04, $03, $3A, $00, $10, $00, $00, $00, $00, $00, $1E, $00, $00, $0A, $32, $00, $10, $00,
      $02, $00, $00, $00, $A6, $0A, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00, $F8, $FF, $FF, $FF, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $2B, $00, $00, $05, $82, $00, $10, $00,
      $00, $00, $00, $00, $0A, $00, $10, $00, $02, $00, $00, $00, $0E, $00, $00, $08, $82, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00, $00, $00, $00, $00, $0A, $80, $20, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $20, $00, $07, $12, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00, $00, $00, $00, $00, $0A, $10, $10, $00, $00, $00, $00, $00, $45, $00, $00, $09, $F2, $00, $10, $00,
      $03, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00, $00, $00, $00, $00, $00, $60, $10, $00, $00, $00, $00, $00, $32, $00, $00, $0A, $F2, $00, $10, $00, $01, $00, $00, $00,
      $46, $0E, $10, $00, $03, $00, $00, $00, $06, $90, $90, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $01, $00, $00, $00, $36, $00, $00, $05, $42, $00, $10, $00, $00, $00, $00, $00,
      $1A, $00, $10, $00, $02, $00, $00, $00, $16, $00, $00, $01, $36, $00, $00, $05, $F2, $20, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $01, $00, $00, $00, $3E, $00, $00, $01, $53, $54, $41, $54,
      $74, $00, $00, $00, $10, $00, $00, $00, $04, $00, $00, $00, $13, $00, $00, $00, $02, $00, $00, $00, $02, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $01, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $0A, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $52, $44, $45, $46, $08, $01, $00, $00, $01, $00, $00, $00, $8C, $00, $00, $00, $03, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FF, $FF, $00, $11, $00, $00, $D4, $00, $00, $00, $7C, $00, $00, $00,
      $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $7C, $00, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00,
      $04, $00, $00, $00, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $01, $00, $00, $00, $0C, $00, $00, $00, $82, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $49, $6E, $70, $75, $74, $00, $24, $47, $6C, $6F, $62, $61, $6C, $73, $00, $AB, $82, $00, $00, $00, $01, $00, $00, $00, $A4, $00, $00, $00,
      $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $BC, $00, $00, $00, $00, $00, $00, $00, $04, $00, $00, $00, $02, $00, $00, $00, $C4, $00, $00, $00, $00, $00, $00, $00, $57, $69, $64, $74,
      $68, $00, $AB, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $48, $4C, $53, $4C, $20, $53,
      $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E, $39, $35, $32, $2E, $32, $38, $34, $34, $00, $AB, $AB, $AB, $49, $53, $47, $4E, $2C, $00, $00, $00,
      $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $03, $03, $00, $00, $54, $45, $58, $43, $4F, $4F, $52, $44,
      $00, $AB, $AB, $AB, $4F, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00,
      $0F, $00, $00, $00, $53, $56, $5F, $54, $61, $72, $67, $65, $74, $00, $AB, $AB], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 4)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.Metal'}
    {$IF defined(MACOS)}
    TContextShaderSource.Create(
      TContextShaderArch.Metal,
      TEncoding.UTF8.GetBytes(
        'using namespace metal;'+

        'struct ProjectedVertex {'+
          'float4 position [[position]];'+
          'float2 textureCoord;'+
        '};'+

        'constant int g_cKernelSize = 19;'+
        'constant float weights[19] = {'+
          '0.038872539997,'+
          '0.043173480779,'+
          '0.047361940145,'+
          '0.051319248974,'+
          '0.054924920201,'+
          '0.058062653989,'+
          '0.060626529157,'+
          '0.062526896596,'+
          '0.063695587218,'+
          '0.038872539997,'+
          '0.063695587218,'+
          '0.062526896596,'+
          '0.060626529157,'+
          '0.058062653989,'+
          '0.054924920201,'+
          '0.051319248974,'+
          '0.047361940145,'+
          '0.043173480779,'+
          '0.038872539997'+
        '};'+

        'fragment float4 fragmentShader(const ProjectedVertex in [[stage_in]],'+
                                       'constant float4 &Width [[buffer(0)]],'+
                                       'const texture2d<float> Input [[texture(0)]],'+
                                       'const sampler InputSampler [[sampler(0)]]) {'+
          'float4 Color = 0;'+
          'float2 newCoord = 0;'+

          'for (int i = 0; i < g_cKernelSize; i++)'+
          '{'+
            'newCoord = in.textureCoord;'+
            'newCoord.x = in.textureCoord.x + ((i - (g_cKernelSize / 2) + 1) / Width.x);'+
            'newCoord = clamp(newCoord, 0, 1);'+
            'Color += Input.sample(InputSampler, newCoord) * weights[i];'+
          '}'+
          'return Color;'+
        '}'
      ),
      [TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1),
       TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.GLSL'}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0A, $0D, $2F, $2F, $20, $67, $6C, $73, $6C, $66, $20, $6F, $75, $74, $70, $75, $74, $20, $62, $79, $20, $43,
      $67, $20, $63, $6F, $6D, $70, $69, $6C, $65, $72, $0A, $2F, $2F, $20, $63, $67, $63, $20, $76, $65, $72, $73, $69, $6F, $6E, $20, $33, $2E, $30, $2E, $30, $30, $31, $36, $2C, $20, $62, $75, $69, $6C,
      $64, $20, $64, $61, $74, $65, $20, $46, $65, $62, $20, $31, $31, $20, $32, $30, $31, $31, $0A, $2F, $2F, $20, $63, $6F, $6D, $6D, $61, $6E, $64, $20, $6C, $69, $6E, $65, $20, $61, $72, $67, $73, $3A,
      $20, $2D, $71, $20, $2D, $70, $72, $6F, $66, $69, $6C, $65, $20, $67, $6C, $73, $6C, $66, $20, $2D, $65, $6E, $74, $72, $79, $20, $6D, $61, $69, $6E, $0A, $2F, $2F, $20, $73, $6F, $75, $72, $63, $65,
      $20, $66, $69, $6C, $65, $3A, $20, $67, $61, $75, $73, $73, $69, $61, $6E, $62, $6C, $75, $72, $68, $2E, $66, $70, $73, $0A, $2F, $2F, $76, $65, $6E, $64, $6F, $72, $20, $4E, $56, $49, $44, $49, $41,
      $20, $43, $6F, $72, $70, $6F, $72, $61, $74, $69, $6F, $6E, $0A, $2F, $2F, $76, $65, $72, $73, $69, $6F, $6E, $20, $33, $2E, $30, $2E, $30, $2E, $31, $36, $0A, $2F, $2F, $70, $72, $6F, $66, $69, $6C,
      $65, $20, $67, $6C, $73, $6C, $66, $0A, $2F, $2F, $70, $72, $6F, $67, $72, $61, $6D, $20, $6D, $61, $69, $6E, $0A, $2F, $2F, $73, $65, $6D, $61, $6E, $74, $69, $63, $20, $49, $6E, $70, $75, $74, $0A,
      $2F, $2F, $73, $65, $6D, $61, $6E, $74, $69, $63, $20, $57, $69, $64, $74, $68, $0A, $2F, $2F, $76, $61, $72, $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $49, $6E, $70, $75, $74, $20, $3A,
      $20, $20, $3A, $20, $5F, $49, $6E, $70, $75, $74, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20, $66, $6C, $6F, $61, $74, $20, $57, $69, $64, $74, $68, $20, $3A, $20,
      $20, $3A, $20, $5F, $57, $69, $64, $74, $68, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20, $66, $6C, $6F, $61, $74, $32, $20, $74, $65, $78, $43, $6F, $6F, $72, $64,
      $20, $3A, $20, $24, $76, $69, $6E, $2E, $54, $45, $58, $43, $4F, $4F, $52, $44, $20, $3A, $20, $54, $45, $58, $30, $20, $3A, $20, $30, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20, $66, $6C,
      $6F, $61, $74, $34, $20, $6D, $61, $69, $6E, $20, $3A, $20, $24, $76, $6F, $75, $74, $2E, $43, $4F, $4C, $4F, $52, $20, $3A, $20, $43, $4F, $4C, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A, $0A,
      $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $31, $3B, $0A, $76, $65, $63, $32, $20, $5F, $54, $4D, $50, $32, $3B, $0A, $76, $65, $63, $32, $20, $5F, $54, $4D, $50, $36, $3B, $0A, $75, $6E, $69, $66,
      $6F, $72, $6D, $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $5F, $49, $6E, $70, $75, $74, $3B, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $5F, $57, $69, $64,
      $74, $68, $3B, $0A, $0A, $20, $2F, $2F, $20, $6D, $61, $69, $6E, $20, $70, $72, $6F, $63, $65, $64, $75, $72, $65, $2C, $20, $74, $68, $65, $20, $6F, $72, $69, $67, $69, $6E, $61, $6C, $20, $6E, $61,
      $6D, $65, $20, $77, $61, $73, $20, $6D, $61, $69, $6E, $0A, $76, $6F, $69, $64, $20, $6D, $61, $69, $6E, $28, $29, $0A, $7B, $0A, $0A, $20, $20, $20, $20, $76, $65, $63, $34, $20, $5F, $43, $6F, $6C,
      $6F, $72, $3B, $0A, $20, $20, $20, $20, $76, $65, $63, $32, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $3B, $0A, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D,
      $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $2D, $38, $2E,
      $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65,
      $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F,
      $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30,
      $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20,
      $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43,
      $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $54, $4D, $50, $31, $2A, $33, $2E, $38, $38, $37, $32, $35, $34, $30, $30, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20,
      $2B, $20, $2D, $37, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D,
      $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30,
      $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E,
      $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B,
      $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20,
      $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $34, $2E, $33, $31, $37, $33, $34, $38, $30, $38, $45, $2D, $30,
      $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F,
      $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $2D, $36, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B,
      $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20,
      $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20,
      $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45,
      $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E,
      $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31,
      $2A, $34, $2E, $37, $33, $36, $31, $39, $34, $30, $31, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78,
      $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $2D, $35, $2E, $30, $30, $30, $30, $30, $30, $30,
      $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30,
      $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72,
      $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30,
      $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D,
      $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20,
      $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $35, $2E, $31, $33, $31, $39, $32, $34, $39, $30, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77,
      $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E,
      $78, $20, $2B, $20, $2D, $34, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D,
      $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B,
      $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20,
      $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32,
      $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B,
      $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $35, $2E, $34, $39, $32, $34, $39, $32, $30, $32, $45,
      $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43,
      $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $2D, $33, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74,
      $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30,
      $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50,
      $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30,
      $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F,
      $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D,
      $50, $31, $2A, $35, $2E, $38, $30, $36, $32, $36, $35, $34, $30, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30,
      $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $2D, $32, $2E, $30, $30, $30, $30, $30,
      $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31,
      $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B,
      $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31,
      $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20,
      $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $36, $2E, $30, $36, $32, $36, $35, $32, $39, $32, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E,
      $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58,
      $30, $2E, $78, $20, $2B, $20, $2D, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32,
      $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30,
      $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32,
      $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D,
      $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36,
      $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $36, $2E, $32, $35, $32, $36, $38, $39, $36,
      $36, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65,
      $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64,
      $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30,
      $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D,
      $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30,
      $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28,
      $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54,
      $4D, $50, $31, $2A, $36, $2E, $33, $36, $39, $35, $35, $38, $37, $32, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58,
      $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $31, $2E, $30, $30, $30, $30, $30,
      $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31,
      $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B,
      $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31,
      $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20,
      $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $33, $2E, $38, $38, $37, $32, $35, $34, $30, $30, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E,
      $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58,
      $30, $2E, $78, $20, $2B, $20, $32, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20,
      $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45,
      $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28,
      $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50,
      $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29,
      $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $36, $2E, $33, $36, $39, $35, $35, $38, $37, $32,
      $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77,
      $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $33, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74,
      $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30,
      $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50,
      $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30,
      $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F,
      $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D,
      $50, $31, $2A, $36, $2E, $32, $35, $32, $36, $38, $39, $36, $36, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30,
      $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $34, $2E, $30, $30, $30, $30, $30, $30,
      $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E,
      $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F,
      $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30,
      $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20,
      $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D,
      $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $36, $2E, $30, $36, $32, $36, $35, $32, $39, $32, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65,
      $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30,
      $2E, $78, $20, $2B, $20, $35, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D,
      $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B,
      $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20,
      $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32,
      $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B,
      $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $35, $2E, $38, $30, $36, $32, $36, $35, $34, $30, $45,
      $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43,
      $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $36, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68,
      $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C,
      $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36,
      $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30,
      $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49,
      $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50,
      $31, $2A, $35, $2E, $34, $39, $32, $34, $39, $32, $30, $32, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E,
      $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $37, $2E, $30, $30, $30, $30, $30, $30, $30,
      $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30,
      $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72,
      $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30,
      $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D,
      $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20,
      $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $35, $2E, $31, $33, $31, $39, $32, $34, $39, $30, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77,
      $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E,
      $78, $20, $2B, $20, $38, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20,
      $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30,
      $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30,
      $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29,
      $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A,
      $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $34, $2E, $37, $33, $36, $31, $39, $34, $30, $31, $45, $2D,
      $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $39, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B,
      $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20,
      $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20,
      $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45,
      $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E,
      $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31,
      $2A, $34, $2E, $33, $31, $37, $33, $34, $38, $30, $38, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78,
      $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30,
      $45, $2B, $30, $30, $31, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30,
      $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64,
      $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30,
      $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20,
      $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F,
      $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $33, $2E, $38, $38, $37, $32, $35, $34, $30, $30, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $67, $6C, $5F, $46, $72,
      $61, $67, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $3B, $0A, $20, $20, $20, $20, $72, $65, $74, $75, $72, $6E, $3B, $0A, $7D, $20, $2F, $2F, $20, $6D, $61, $69, $6E, $20,
      $65, $6E, $64, $0A], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1)]
    )
    {$ENDREGION}

  ]);
  FShaders[1] := TShaderManager.RegisterShaderFromData('gaussianblurv.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.SKSL'}
    TContextShaderSource.Create(TContextShaderArch.SKSL, [
      $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $68, $61, $64, $65, $72, 
      $20, $49, $6E, $70, $75, $74, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, 
      $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $57, $69, $64, $74, $68, $3B, $69, $6E, $6C, $69, $6E, $65, $20, $66, $6C, $6F, $61, $74, $20, $61, $28, $63, $6F, $6E, $73, $74, $20, $69, 
      $6E, $74, $20, $62, $29, $7B, $73, $77, $69, $74, $63, $68, $28, $62, $29, $7B, $63, $61, $73, $65, $20, $30, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $33, $38, $38, $37, $32, $35, $34, $3B, $63, 
      $61, $73, $65, $20, $31, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $34, $33, $31, $37, $33, $34, $38, $3B, $63, $61, $73, $65, $20, $32, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $34, $37, $33, 
      $36, $31, $39, $34, $3B, $63, $61, $73, $65, $20, $33, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $35, $31, $33, $31, $39, $32, $35, $3B, $63, $61, $73, $65, $20, $34, $3A, $72, $65, $74, $75, $72, 
      $6E, $2E, $30, $35, $34, $39, $32, $34, $39, $32, $3B, $63, $61, $73, $65, $20, $35, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $35, $38, $30, $36, $32, $36, $35, $34, $3B, $63, $61, $73, $65, $20, 
      $36, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $36, $30, $36, $32, $36, $35, $33, $3B, $63, $61, $73, $65, $20, $37, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $36, $32, $35, $32, $36, $39, $3B, 
      $63, $61, $73, $65, $20, $38, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $36, $33, $36, $39, $35, $35, $39, $3B, $63, $61, $73, $65, $20, $39, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $33, $38, 
      $38, $37, $32, $35, $34, $3B, $63, $61, $73, $65, $20, $31, $30, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $36, $33, $36, $39, $35, $35, $39, $3B, $63, $61, $73, $65, $20, $31, $31, $3A, $72, $65, 
      $74, $75, $72, $6E, $2E, $30, $36, $32, $35, $32, $36, $39, $3B, $63, $61, $73, $65, $20, $31, $32, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $36, $30, $36, $32, $36, $35, $33, $3B, $63, $61, $73, 
      $65, $20, $31, $33, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $35, $38, $30, $36, $32, $36, $35, $34, $3B, $63, $61, $73, $65, $20, $31, $34, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $35, $34, 
      $39, $32, $34, $39, $32, $3B, $63, $61, $73, $65, $20, $31, $35, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $35, $31, $33, $31, $39, $32, $35, $3B, $63, $61, $73, $65, $20, $31, $36, $3A, $72, $65, 
      $74, $75, $72, $6E, $2E, $30, $34, $37, $33, $36, $31, $39, $34, $3B, $63, $61, $73, $65, $20, $31, $37, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $34, $33, $31, $37, $33, $34, $38, $3B, $64, $65, 
      $66, $61, $75, $6C, $74, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $33, $38, $38, $37, $32, $35, $34, $3B, $7D, $7D, $68, $61, $6C, $66, $34, $20, $6D, $61, $69, $6E, $28, $66, $6C, $6F, $61, $74, 
      $32, $20, $62, $29, $7B, $66, $6C, $6F, $61, $74, $32, $20, $63, $3D, $62, $2F, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $66, $6C, $6F, $61, $74, $34, $20, $64, $3D, $66, $6C, $6F, $61, 
      $74, $34, $28, $30, $2E, $29, $3B, $66, $6F, $72, $28, $69, $6E, $74, $20, $65, $3D, $30, $3B, $65, $3C, $31, $39, $3B, $65, $2B, $2B, $29, $7B, $66, $6C, $6F, $61, $74, $32, $20, $66, $3D, $63, $3B, 
      $66, $2E, $79, $2B, $3D, $28, $28, $66, $6C, $6F, $61, $74, $28, $65, $29, $2D, $39, $2E, $35, $29, $2B, $31, $2E, $29, $2F, $57, $69, $64, $74, $68, $3B, $64, $2B, $3D, $66, $6C, $6F, $61, $74, $34, 
      $28, $49, $6E, $70, $75, $74, $2E, $65, $76, $61, $6C, $28, $66, $2A, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $29, $29, $2A, $61, $28, $65, $29, $3B, $7D, $72, $65, 
      $74, $75, $72, $6E, $20, $68, $61, $6C, $66, $34, $28, $64, $29, $3B, $7D, $0A, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Resolution', TContextShaderVariableKind.Float2, 0, 8),
      TContextShaderVariable.Create('InputResolution', TContextShaderVariableKind.Float2, 1, 8),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 2, 4)]
    ),
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FF, $FF, $FE, $FF, $29, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $7B, $00, $00, $00, $00, $02, $FF, $FF, $02, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $74, $00, $00, $00,
      $44, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $4C, $00, $00, $00, $00, $00, $00, $00, $5C, $00, $00, $00, $02, $00, $00, $00, $01, $00, $00, $00, $64, $00, $00, $00, $00, $00, $00, $00,
      $49, $6E, $70, $75, $74, $00, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $57, $69, $64, $74, $68, $00, $AB, $AB, $00, $00, $03, $00, $01, $00, $01, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65,
      $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $51, $00, $00, $05, $01, $00, $0F, $A0, $00, $00, $C0, $C0, $98, $FE, $41, $3D, $00, $00, $A0, $C0, $22, $34, $52, $3D, $51, $00, $00, $05,
      $02, $00, $0F, $A0, $00, $00, $00, $C0, $86, $53, $78, $3D, $1A, $0E, $80, $3D, $D5, $72, $82, $3D, $51, $00, $00, $05, $03, $00, $0F, $A0, $00, $00, $80, $C0, $F4, $F8, $60, $3D, $00, $00, $40, $C0,
      $1B, $D3, $6D, $3D, $51, $00, $00, $05, $04, $00, $0F, $A0, $00, $00, $00, $C1, $00, $00, $E0, $C0, $AD, $D6, $30, $3D, $D0, $38, $1F, $3D, $51, $00, $00, $05, $05, $00, $0F, $A0, $00, $00, $10, $41,
      $00, $00, $20, $41, $00, $00, $00, $00, $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $01, $00, $00, $02,
      $00, $00, $11, $80, $00, $00, $00, $B0, $06, $00, $00, $02, $00, $00, $04, $80, $00, $00, $00, $A0, $04, $00, $00, $04, $00, $00, $12, $80, $00, $00, $AA, $80, $04, $00, $55, $A0, $00, $00, $55, $B0,
      $01, $00, $00, $02, $01, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $01, $00, $12, $80, $00, $00, $AA, $80, $04, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $02, $00, $11, $80,
      $00, $00, $00, $B0, $04, $00, $00, $04, $02, $00, $12, $80, $00, $00, $AA, $80, $01, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $03, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04,
      $03, $00, $12, $80, $00, $00, $AA, $80, $01, $00, $AA, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $04, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $04, $00, $12, $80, $00, $00, $AA, $80,
      $03, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $05, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $05, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $AA, $A0, $00, $00, $55, $B0,
      $01, $00, $00, $02, $06, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $06, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $07, $00, $11, $80,
      $00, $00, $00, $B0, $02, $00, $00, $03, $07, $00, $12, $80, $00, $00, $AA, $81, $00, $00, $55, $B0, $01, $00, $00, $02, $08, $00, $13, $80, $00, $00, $E4, $B0, $42, $00, $00, $03, $09, $00, $0F, $80,
      $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80,
      $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $08, $00, $0F, $80, $08, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03, $09, $00, $0F, $80, $09, $00, $E4, $80, $04, $00, $AA, $A0, $04, $00, $00, $04, $01, $00, $0F, $80,
      $01, $00, $E4, $80, $04, $00, $FF, $A0, $09, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $02, $00, $E4, $80, $01, $00, $55, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80,
      $03, $00, $E4, $80, $01, $00, $FF, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $04, $00, $E4, $80, $03, $00, $55, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80,
      $05, $00, $E4, $80, $03, $00, $FF, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $06, $00, $E4, $80, $02, $00, $55, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80,
      $07, $00, $E4, $80, $02, $00, $AA, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $08, $00, $E4, $80, $02, $00, $FF, $A0, $01, $00, $E4, $80, $01, $00, $00, $02, $00, $00, $11, $80,
      $00, $00, $00, $B0, $02, $00, $00, $03, $00, $00, $12, $80, $00, $00, $AA, $80, $00, $00, $55, $B0, $01, $00, $00, $02, $02, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $02, $00, $12, $80,
      $00, $00, $AA, $80, $02, $00, $00, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $03, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $03, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $AA, $A1,
      $00, $00, $55, $B0, $01, $00, $00, $02, $04, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $04, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $00, $A1, $00, $00, $55, $B0, $01, $00, $00, $02,
      $05, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $05, $00, $12, $80, $00, $00, $AA, $80, $01, $00, $AA, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $06, $00, $11, $80, $00, $00, $00, $B0,
      $04, $00, $00, $04, $06, $00, $12, $80, $00, $00, $AA, $80, $01, $00, $00, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $07, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $07, $00, $12, $80,
      $00, $00, $AA, $80, $04, $00, $55, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $08, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $08, $00, $12, $80, $00, $00, $AA, $80, $04, $00, $00, $A1,
      $00, $00, $55, $B0, $42, $00, $00, $03, $09, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
      $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80,
      $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
      $08, $00, $0F, $80, $08, $00, $E4, $80, $00, $08, $E4, $A0, $04, $00, $00, $04, $01, $00, $0F, $80, $09, $00, $E4, $80, $04, $00, $FF, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80,
      $02, $00, $E4, $80, $02, $00, $FF, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $03, $00, $E4, $80, $02, $00, $AA, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80,
      $04, $00, $E4, $80, $02, $00, $55, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $05, $00, $E4, $80, $03, $00, $FF, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80,
      $06, $00, $E4, $80, $03, $00, $55, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $07, $00, $E4, $80, $01, $00, $FF, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80,
      $08, $00, $E4, $80, $01, $00, $55, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $00, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $00, $A0, $00, $00, $55, $B0, $04, $00, $00, $04, $02, $00, $12, $80,
      $00, $00, $AA, $80, $05, $00, $55, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $00, $00, $11, $80, $00, $00, $00, $B0, $01, $00, $00, $02, $02, $00, $11, $80, $00, $00, $00, $B0, $42, $00, $00, $03,
      $00, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $04, $00, $00, $04, $00, $00, $0F, $80, $00, $00, $E4, $80,
      $04, $00, $AA, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $00, $00, $0F, $80, $02, $00, $E4, $80, $04, $00, $FF, $A0, $00, $00, $E4, $80, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80,
      $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX11_level_9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $5C, $55, $FE, $41, $40, $F2, $4B, $EF, $4C, $19, $25, $62, $1C, $F4, $86, $FC, $01, $00, $00, $00, $00, $0B, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $10, $06, $00, $00,
      $0C, $09, $00, $00, $88, $09, $00, $00, $98, $0A, $00, $00, $CC, $0A, $00, $00, $41, $6F, $6E, $39, $D0, $05, $00, $00, $D0, $05, $00, $00, $00, $02, $FF, $FF, $9C, $05, $00, $00, $34, $00, $00, $00,
      $01, $00, $28, $00, $00, $00, $34, $00, $00, $00, $34, $00, $01, $00, $24, $00, $00, $00, $34, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $02, $FF, $FF,
      $51, $00, $00, $05, $01, $00, $0F, $A0, $00, $00, $00, $C1, $00, $00, $E0, $C0, $AD, $D6, $30, $3D, $D0, $38, $1F, $3D, $51, $00, $00, $05, $02, $00, $0F, $A0, $00, $00, $C0, $C0, $98, $FE, $41, $3D,
      $00, $00, $A0, $C0, $22, $34, $52, $3D, $51, $00, $00, $05, $03, $00, $0F, $A0, $00, $00, $80, $C0, $F4, $F8, $60, $3D, $00, $00, $40, $C0, $1B, $D3, $6D, $3D, $51, $00, $00, $05, $04, $00, $0F, $A0,
      $00, $00, $00, $C0, $86, $53, $78, $3D, $1A, $0E, $80, $3D, $D5, $72, $82, $3D, $51, $00, $00, $05, $05, $00, $0F, $A0, $00, $00, $10, $41, $00, $00, $20, $41, $00, $00, $00, $00, $00, $00, $00, $00,
      $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $01, $00, $00, $02, $00, $00, $11, $80, $00, $00, $00, $B0, $06, $00, $00, $02,
      $00, $00, $04, $80, $00, $00, $00, $A0, $04, $00, $00, $04, $00, $00, $12, $80, $00, $00, $AA, $80, $01, $00, $55, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $01, $00, $11, $80, $00, $00, $00, $B0,
      $04, $00, $00, $04, $01, $00, $12, $80, $00, $00, $AA, $80, $01, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $02, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $02, $00, $12, $80,
      $00, $00, $AA, $80, $02, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $03, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $03, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $AA, $A0,
      $00, $00, $55, $B0, $01, $00, $00, $02, $04, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $04, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02,
      $05, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $05, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $AA, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $06, $00, $11, $80, $00, $00, $00, $B0,
      $04, $00, $00, $04, $06, $00, $12, $80, $00, $00, $AA, $80, $04, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $07, $00, $11, $80, $00, $00, $00, $B0, $02, $00, $00, $03, $07, $00, $12, $80,
      $00, $00, $AA, $81, $00, $00, $55, $B0, $01, $00, $00, $02, $08, $00, $13, $80, $00, $00, $E4, $B0, $42, $00, $00, $03, $09, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
      $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80,
      $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
      $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $08, $00, $0F, $80, $08, $00, $E4, $80,
      $00, $08, $E4, $A0, $05, $00, $00, $03, $09, $00, $0F, $80, $09, $00, $E4, $80, $01, $00, $AA, $A0, $04, $00, $00, $04, $01, $00, $0F, $80, $01, $00, $E4, $80, $01, $00, $FF, $A0, $09, $00, $E4, $80,
      $04, $00, $00, $04, $01, $00, $0F, $80, $02, $00, $E4, $80, $02, $00, $55, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $03, $00, $E4, $80, $02, $00, $FF, $A0, $01, $00, $E4, $80,
      $04, $00, $00, $04, $01, $00, $0F, $80, $04, $00, $E4, $80, $03, $00, $55, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $05, $00, $E4, $80, $03, $00, $FF, $A0, $01, $00, $E4, $80,
      $04, $00, $00, $04, $01, $00, $0F, $80, $06, $00, $E4, $80, $04, $00, $55, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $07, $00, $E4, $80, $04, $00, $AA, $A0, $01, $00, $E4, $80,
      $04, $00, $00, $04, $01, $00, $0F, $80, $08, $00, $E4, $80, $04, $00, $FF, $A0, $01, $00, $E4, $80, $01, $00, $00, $02, $00, $00, $11, $80, $00, $00, $00, $B0, $02, $00, $00, $03, $00, $00, $12, $80,
      $00, $00, $AA, $80, $00, $00, $55, $B0, $01, $00, $00, $02, $02, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $02, $00, $12, $80, $00, $00, $AA, $80, $04, $00, $00, $A1, $00, $00, $55, $B0,
      $01, $00, $00, $02, $03, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $03, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $AA, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $04, $00, $11, $80,
      $00, $00, $00, $B0, $04, $00, $00, $04, $04, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $00, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $05, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04,
      $05, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $AA, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $06, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $06, $00, $12, $80, $00, $00, $AA, $80,
      $02, $00, $00, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $07, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $07, $00, $12, $80, $00, $00, $AA, $80, $01, $00, $55, $A1, $00, $00, $55, $B0,
      $01, $00, $00, $02, $08, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $08, $00, $12, $80, $00, $00, $AA, $80, $01, $00, $00, $A1, $00, $00, $55, $B0, $42, $00, $00, $03, $09, $00, $0F, $80,
      $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80,
      $06, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $08, $00, $0F, $80, $08, $00, $E4, $80, $00, $08, $E4, $A0,
      $04, $00, $00, $04, $01, $00, $0F, $80, $09, $00, $E4, $80, $01, $00, $FF, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $02, $00, $E4, $80, $04, $00, $FF, $A0, $01, $00, $E4, $80,
      $04, $00, $00, $04, $01, $00, $0F, $80, $03, $00, $E4, $80, $04, $00, $AA, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $04, $00, $E4, $80, $04, $00, $55, $A0, $01, $00, $E4, $80,
      $04, $00, $00, $04, $01, $00, $0F, $80, $05, $00, $E4, $80, $03, $00, $FF, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $06, $00, $E4, $80, $03, $00, $55, $A0, $01, $00, $E4, $80,
      $04, $00, $00, $04, $01, $00, $0F, $80, $07, $00, $E4, $80, $02, $00, $FF, $A0, $01, $00, $E4, $80, $04, $00, $00, $04, $01, $00, $0F, $80, $08, $00, $E4, $80, $02, $00, $55, $A0, $01, $00, $E4, $80,
      $04, $00, $00, $04, $00, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $00, $A0, $00, $00, $55, $B0, $04, $00, $00, $04, $02, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $55, $A0, $00, $00, $55, $B0,
      $01, $00, $00, $02, $00, $00, $11, $80, $00, $00, $00, $B0, $01, $00, $00, $02, $02, $00, $11, $80, $00, $00, $00, $B0, $42, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $04, $00, $00, $04, $00, $00, $0F, $80, $00, $00, $E4, $80, $01, $00, $AA, $A0, $01, $00, $E4, $80, $04, $00, $00, $04,
      $00, $00, $0F, $80, $02, $00, $E4, $80, $01, $00, $FF, $A0, $00, $00, $E4, $80, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00, $53, $48, $44, $52, $F4, $02, $00, $00,
      $40, $00, $00, $00, $BD, $00, $00, $00, $35, $18, $00, $00, $4E, $00, $00, $00, $D0, $38, $1F, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $AD, $D6, $30, $3D, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $98, $FE, $41, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $22, $34, $52, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $F4, $F8, $60, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $1B, $D3, $6D, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $86, $53, $78, $3D, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $1A, $0E, $80, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $D5, $72, $82, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $D0, $38, $1F, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $D5, $72, $82, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $1A, $0E, $80, $3D, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $86, $53, $78, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $1B, $D3, $6D, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $F4, $F8, $60, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $22, $34, $52, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $98, $FE, $41, $3D, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $AD, $D6, $30, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $D0, $38, $1F, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $59, $00, $00, $04, $46, $8E, $20, $00, $00, $00, $00, $00, $01, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $00, $00, $00, $00, $58, $18, $00, $04, $00, $70, $10, $00, $00, $00, $00, $00,
      $55, $55, $00, $00, $62, $10, $00, $03, $32, $10, $10, $00, $00, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00, $00, $00, $00, $00, $68, $00, $00, $02, $04, $00, $00, $00, $36, $20, $00, $05,
      $12, $00, $10, $00, $00, $00, $00, $00, $0A, $10, $10, $00, $00, $00, $00, $00, $36, $00, $00, $08, $F2, $00, $10, $00, $01, $00, $00, $00, $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $36, $00, $00, $05, $42, $00, $10, $00, $00, $00, $00, $00, $01, $40, $00, $00, $00, $00, $00, $00, $30, $00, $00, $01, $21, $00, $00, $07, $82, $00, $10, $00,
      $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $01, $40, $00, $00, $13, $00, $00, $00, $03, $00, $04, $03, $3A, $00, $10, $00, $00, $00, $00, $00, $1E, $00, $00, $0A, $32, $00, $10, $00,
      $02, $00, $00, $00, $A6, $0A, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00, $F8, $FF, $FF, $FF, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $2B, $00, $00, $05, $82, $00, $10, $00,
      $00, $00, $00, $00, $0A, $00, $10, $00, $02, $00, $00, $00, $0E, $00, $00, $08, $82, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00, $00, $00, $00, $00, $0A, $80, $20, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $20, $00, $07, $22, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00, $00, $00, $00, $00, $1A, $10, $10, $00, $00, $00, $00, $00, $45, $00, $00, $09, $F2, $00, $10, $00,
      $03, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00, $00, $00, $00, $00, $00, $60, $10, $00, $00, $00, $00, $00, $32, $00, $00, $0A, $F2, $00, $10, $00, $01, $00, $00, $00,
      $46, $0E, $10, $00, $03, $00, $00, $00, $06, $90, $90, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $01, $00, $00, $00, $36, $00, $00, $05, $42, $00, $10, $00, $00, $00, $00, $00,
      $1A, $00, $10, $00, $02, $00, $00, $00, $16, $00, $00, $01, $36, $00, $00, $05, $F2, $20, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $01, $00, $00, $00, $3E, $00, $00, $01, $53, $54, $41, $54,
      $74, $00, $00, $00, $10, $00, $00, $00, $04, $00, $00, $00, $13, $00, $00, $00, $02, $00, $00, $00, $02, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $01, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $0A, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $52, $44, $45, $46, $08, $01, $00, $00, $01, $00, $00, $00, $8C, $00, $00, $00, $03, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FF, $FF, $00, $11, $00, $00, $D4, $00, $00, $00, $7C, $00, $00, $00,
      $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $7C, $00, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00,
      $04, $00, $00, $00, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $01, $00, $00, $00, $0C, $00, $00, $00, $82, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $49, $6E, $70, $75, $74, $00, $24, $47, $6C, $6F, $62, $61, $6C, $73, $00, $AB, $82, $00, $00, $00, $01, $00, $00, $00, $A4, $00, $00, $00,
      $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $BC, $00, $00, $00, $00, $00, $00, $00, $04, $00, $00, $00, $02, $00, $00, $00, $C4, $00, $00, $00, $00, $00, $00, $00, $57, $69, $64, $74,
      $68, $00, $AB, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $48, $4C, $53, $4C, $20, $53,
      $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E, $39, $35, $32, $2E, $32, $38, $34, $34, $00, $AB, $AB, $AB, $49, $53, $47, $4E, $2C, $00, $00, $00,
      $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $03, $03, $00, $00, $54, $45, $58, $43, $4F, $4F, $52, $44,
      $00, $AB, $AB, $AB, $4F, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00,
      $0F, $00, $00, $00, $53, $56, $5F, $54, $61, $72, $67, $65, $74, $00, $AB, $AB], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 4)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.Metal'}
    {$IF defined(MACOS)}
    TContextShaderSource.Create(
      TContextShaderArch.Metal,
      TEncoding.UTF8.GetBytes(
        'using namespace metal;'+

        'struct ProjectedVertex {'+
          'float4 position [[position]];'+
          'float2 textureCoord;'+
        '};'+

        'constant int g_cKernelSize = 19;'+
        'constant float weights[19] = {'+
          '0.038872539997,'+
          '0.043173480779,'+
          '0.047361940145,'+
          '0.051319248974,'+
          '0.054924920201,'+
          '0.058062653989,'+
          '0.060626529157,'+
          '0.062526896596,'+
          '0.063695587218,'+
          '0.038872539997,'+
          '0.063695587218,'+
          '0.062526896596,'+
          '0.060626529157,'+
          '0.058062653989,'+
          '0.054924920201,'+
          '0.051319248974,'+
          '0.047361940145,'+
          '0.043173480779,'+
          '0.038872539997'+
        '};'+

        'fragment float4 fragmentShader(const ProjectedVertex in [[stage_in]],'+
                                       'constant float4 &Width [[buffer(0)]],'+
                                       'const texture2d<float> Input [[texture(0)]],'+
                                       'const sampler InputSampler [[sampler(0)]]) {'+
          'float4 Color = 0;'+
          'float2 newCoord = 0;'+

          'for (int i = 0; i < g_cKernelSize; i++)'+
          '{'+
            'newCoord = in.textureCoord;'+
            'newCoord.y = in.textureCoord.y + ((i - (g_cKernelSize / 2) + 1) / Width.x);'+
            'newCoord = clamp(newCoord, 0, 1);'+
            'Color += Input.sample(InputSampler, newCoord) * weights[i];'+
          '}'+
          'return Color;'+
        '}'
      ),
      [TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1),
       TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.GLSL'}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0A, $0D, $2F, $2F, $20, $67, $6C, $73, $6C, $66, $20, $6F, $75, $74, $70, $75, $74, $20, $62, $79, $20, $43,
      $67, $20, $63, $6F, $6D, $70, $69, $6C, $65, $72, $0A, $2F, $2F, $20, $63, $67, $63, $20, $76, $65, $72, $73, $69, $6F, $6E, $20, $33, $2E, $30, $2E, $30, $30, $31, $36, $2C, $20, $62, $75, $69, $6C,
      $64, $20, $64, $61, $74, $65, $20, $46, $65, $62, $20, $31, $31, $20, $32, $30, $31, $31, $0A, $2F, $2F, $20, $63, $6F, $6D, $6D, $61, $6E, $64, $20, $6C, $69, $6E, $65, $20, $61, $72, $67, $73, $3A,
      $20, $2D, $71, $20, $2D, $70, $72, $6F, $66, $69, $6C, $65, $20, $67, $6C, $73, $6C, $66, $20, $2D, $65, $6E, $74, $72, $79, $20, $6D, $61, $69, $6E, $0A, $2F, $2F, $20, $73, $6F, $75, $72, $63, $65,
      $20, $66, $69, $6C, $65, $3A, $20, $67, $61, $75, $73, $73, $69, $61, $6E, $62, $6C, $75, $72, $76, $2E, $66, $70, $73, $0A, $2F, $2F, $76, $65, $6E, $64, $6F, $72, $20, $4E, $56, $49, $44, $49, $41,
      $20, $43, $6F, $72, $70, $6F, $72, $61, $74, $69, $6F, $6E, $0A, $2F, $2F, $76, $65, $72, $73, $69, $6F, $6E, $20, $33, $2E, $30, $2E, $30, $2E, $31, $36, $0A, $2F, $2F, $70, $72, $6F, $66, $69, $6C,
      $65, $20, $67, $6C, $73, $6C, $66, $0A, $2F, $2F, $70, $72, $6F, $67, $72, $61, $6D, $20, $6D, $61, $69, $6E, $0A, $2F, $2F, $73, $65, $6D, $61, $6E, $74, $69, $63, $20, $49, $6E, $70, $75, $74, $0A,
      $2F, $2F, $73, $65, $6D, $61, $6E, $74, $69, $63, $20, $57, $69, $64, $74, $68, $0A, $2F, $2F, $76, $61, $72, $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $49, $6E, $70, $75, $74, $20, $3A,
      $20, $20, $3A, $20, $5F, $49, $6E, $70, $75, $74, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20, $66, $6C, $6F, $61, $74, $20, $57, $69, $64, $74, $68, $20, $3A, $20,
      $20, $3A, $20, $5F, $57, $69, $64, $74, $68, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20, $66, $6C, $6F, $61, $74, $32, $20, $74, $65, $78, $43, $6F, $6F, $72, $64,
      $20, $3A, $20, $24, $76, $69, $6E, $2E, $54, $45, $58, $43, $4F, $4F, $52, $44, $20, $3A, $20, $54, $45, $58, $30, $20, $3A, $20, $30, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20, $66, $6C,
      $6F, $61, $74, $34, $20, $6D, $61, $69, $6E, $20, $3A, $20, $24, $76, $6F, $75, $74, $2E, $43, $4F, $4C, $4F, $52, $20, $3A, $20, $43, $4F, $4C, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A, $0A,
      $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $31, $3B, $0A, $76, $65, $63, $32, $20, $5F, $54, $4D, $50, $32, $3B, $0A, $76, $65, $63, $32, $20, $5F, $54, $4D, $50, $36, $3B, $0A, $75, $6E, $69, $66,
      $6F, $72, $6D, $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $5F, $49, $6E, $70, $75, $74, $3B, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $5F, $57, $69, $64,
      $74, $68, $3B, $0A, $0A, $20, $2F, $2F, $20, $6D, $61, $69, $6E, $20, $70, $72, $6F, $63, $65, $64, $75, $72, $65, $2C, $20, $74, $68, $65, $20, $6F, $72, $69, $67, $69, $6E, $61, $6C, $20, $6E, $61,
      $6D, $65, $20, $77, $61, $73, $20, $6D, $61, $69, $6E, $0A, $76, $6F, $69, $64, $20, $6D, $61, $69, $6E, $28, $29, $0A, $7B, $0A, $0A, $20, $20, $20, $20, $76, $65, $63, $34, $20, $5F, $43, $6F, $6C,
      $6F, $72, $3B, $0A, $20, $20, $20, $20, $76, $65, $63, $32, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $3B, $0A, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D,
      $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $38, $2E,
      $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65,
      $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F,
      $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30,
      $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20,
      $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43,
      $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $54, $4D, $50, $31, $2A, $33, $2E, $38, $38, $37, $32, $35, $34, $30, $30, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20,
      $2B, $20, $2D, $37, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D,
      $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30,
      $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E,
      $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B,
      $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20,
      $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $34, $2E, $33, $31, $37, $33, $34, $38, $30, $38, $45, $2D, $30,
      $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F,
      $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $36, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B,
      $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20,
      $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20,
      $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45,
      $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E,
      $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31,
      $2A, $34, $2E, $37, $33, $36, $31, $39, $34, $30, $31, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78,
      $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $35, $2E, $30, $30, $30, $30, $30, $30, $30,
      $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30,
      $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72,
      $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30,
      $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D,
      $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20,
      $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $35, $2E, $31, $33, $31, $39, $32, $34, $39, $30, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77,
      $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E,
      $79, $20, $2B, $20, $2D, $34, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D,
      $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B,
      $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20,
      $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32,
      $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B,
      $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $35, $2E, $34, $39, $32, $34, $39, $32, $30, $32, $45,
      $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43,
      $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $33, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74,
      $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30,
      $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50,
      $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30,
      $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F,
      $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D,
      $50, $31, $2A, $35, $2E, $38, $30, $36, $32, $36, $35, $34, $30, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30,
      $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $32, $2E, $30, $30, $30, $30, $30,
      $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31,
      $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B,
      $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31,
      $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20,
      $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $36, $2E, $30, $36, $32, $36, $35, $32, $39, $32, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E,
      $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58,
      $30, $2E, $79, $20, $2B, $20, $2D, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32,
      $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30,
      $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32,
      $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D,
      $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36,
      $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $36, $2E, $32, $35, $32, $36, $38, $39, $36,
      $36, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65,
      $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64,
      $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30,
      $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D,
      $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30,
      $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28,
      $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54,
      $4D, $50, $31, $2A, $36, $2E, $33, $36, $39, $35, $35, $38, $37, $32, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58,
      $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $31, $2E, $30, $30, $30, $30, $30,
      $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31,
      $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B,
      $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31,
      $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20,
      $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $33, $2E, $38, $38, $37, $32, $35, $34, $30, $30, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E,
      $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58,
      $30, $2E, $79, $20, $2B, $20, $32, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20,
      $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45,
      $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28,
      $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50,
      $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29,
      $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $36, $2E, $33, $36, $39, $35, $35, $38, $37, $32,
      $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77,
      $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $33, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74,
      $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30,
      $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50,
      $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30,
      $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F,
      $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D,
      $50, $31, $2A, $36, $2E, $32, $35, $32, $36, $38, $39, $36, $36, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30,
      $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $34, $2E, $30, $30, $30, $30, $30, $30,
      $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E,
      $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F,
      $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30,
      $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20,
      $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D,
      $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $36, $2E, $30, $36, $32, $36, $35, $32, $39, $32, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65,
      $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30,
      $2E, $79, $20, $2B, $20, $35, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D,
      $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B,
      $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20,
      $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32,
      $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B,
      $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $35, $2E, $38, $30, $36, $32, $36, $35, $34, $30, $45,
      $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43,
      $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $36, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68,
      $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C,
      $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36,
      $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30,
      $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49,
      $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50,
      $31, $2A, $35, $2E, $34, $39, $32, $34, $39, $32, $30, $32, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E,
      $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $37, $2E, $30, $30, $30, $30, $30, $30, $30,
      $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30,
      $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72,
      $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30,
      $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D,
      $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20,
      $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $35, $2E, $31, $33, $31, $39, $32, $34, $39, $30, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77,
      $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E,
      $79, $20, $2B, $20, $38, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20,
      $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30,
      $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30,
      $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29,
      $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A,
      $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $34, $2E, $37, $33, $36, $31, $39, $34, $30, $31, $45, $2D,
      $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $39, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B,
      $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20,
      $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20,
      $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45,
      $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E,
      $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31,
      $2A, $34, $2E, $33, $31, $37, $33, $34, $38, $30, $38, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78,
      $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30,
      $45, $2B, $30, $30, $31, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30,
      $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64,
      $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30,
      $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20,
      $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F,
      $43, $6F, $6C, $6F, $72, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2A, $33, $2E, $38, $38, $37, $32, $35, $34, $30, $30, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $67, $6C, $5F, $46, $72,
      $61, $67, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $43, $6F, $6C, $6F, $72, $3B, $0A, $20, $20, $20, $20, $72, $65, $74, $75, $72, $6E, $3B, $0A, $7D, $20, $2F, $2F, $20, $6D, $61, $69, $6E, $20,
      $65, $6E, $64, $0A], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1)]
    )
    {$ENDREGION}

  ]);
  FPassCount := 2;
end;

destructor TGaussianBlurFilter.Destroy;
begin
  inherited;
end;

procedure TGaussianBlurFilter.LoadShaders;
begin
  FilterContext.SetShaders(FVertexShader, FShaders[FPass]);
  if FPass = 0 then
    FilterContext.SetShaderVariable('Width', [Vector3D(InputSize.Width / ValuesAsFloat['BlurAmount'], 0, 0, 0)]);
  if FPass = 1 then
    FilterContext.SetShaderVariable('Width', [Vector3D(InputSize.Height / ValuesAsFloat['BlurAmount'], 0, 0, 0)]);
end;

class function TGaussianBlurFilter.FilterAttr: TFilterRec;
begin
  Result := TFilterRec.Create('GaussianBlur', 'An effect that GaussianBlurs.', [
    TFilterValueRec.Create('BlurAmount', 'The GaussianBlur factor.', TFilterValueType.Float, 1, 0.01, 10)
  ]);
end;

{ TGlowFilter }

constructor TGlowFilter.Create;
begin
  inherited;
  FShaders[0] := TShaderManager.RegisterShaderFromData('glowh.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.SKSL'}
    TContextShaderSource.Create(TContextShaderArch.SKSL, [
      $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $68, $61, $64, $65, $72, 
      $20, $49, $6E, $70, $75, $74, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, 
      $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $57, $69, $64, $74, $68, $3B, $69, $6E, $6C, $69, $6E, $65, $20, $66, $6C, $6F, $61, $74, $20, $61, $28, $63, $6F, $6E, $73, $74, $20, $69, 
      $6E, $74, $20, $62, $29, $7B, $73, $77, $69, $74, $63, $68, $28, $62, $29, $7B, $63, $61, $73, $65, $20, $30, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $33, $34, $35, $33, $37, $38, $36, $33, $3B, 
      $63, $61, $73, $65, $20, $31, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $34, $38, $31, $31, $36, $39, $30, $33, $38, $3B, $63, $61, $73, $65, $20, $32, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, 
      $35, $39, $39, $35, $37, $33, $35, $35, $33, $3B, $63, $61, $73, $65, $20, $33, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $37, $31, $37, $38, $31, $39, $38, $35, $35, $3B, $63, $61, $73, $65, $20, 
      $34, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $38, $32, $35, $36, $38, $39, $33, $36, $3B, $63, $61, $73, $65, $20, $35, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $39, $31, $32, $35, $32, $37, 
      $39, $3B, $63, $61, $73, $65, $20, $36, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $39, $36, $38, $39, $35, $35, $34, $35, $37, $3B, $63, $61, $73, $65, $20, $37, $3A, $72, $65, $74, $75, $72, $6E, 
      $2E, $30, $39, $38, $38, $35, $32, $39, $37, $3B, $63, $61, $73, $65, $20, $38, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $39, $36, $38, $39, $35, $35, $34, $35, $37, $3B, $63, $61, $73, $65, $20, 
      $39, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $39, $31, $32, $35, $32, $37, $39, $3B, $63, $61, $73, $65, $20, $31, $30, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $38, $32, $35, $36, $38, $39, 
      $33, $36, $3B, $63, $61, $73, $65, $20, $31, $31, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $37, $31, $37, $38, $31, $39, $38, $35, $35, $3B, $63, $61, $73, $65, $20, $31, $32, $3A, $72, $65, $74, 
      $75, $72, $6E, $2E, $30, $35, $39, $39, $35, $37, $33, $35, $35, $33, $3B, $63, $61, $73, $65, $20, $31, $33, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $34, $38, $31, $31, $36, $39, $30, $33, $38, 
      $3B, $64, $65, $66, $61, $75, $6C, $74, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $33, $34, $35, $33, $37, $38, $36, $33, $3B, $7D, $7D, $68, $61, $6C, $66, $34, $20, $6D, $61, $69, $6E, $28, $66, 
      $6C, $6F, $61, $74, $32, $20, $62, $29, $7B, $66, $6C, $6F, $61, $74, $32, $20, $63, $3D, $62, $2F, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $66, $6C, $6F, $61, $74, $20, $64, $3D, $30, 
      $2E, $3B, $66, $6F, $72, $28, $69, $6E, $74, $20, $65, $3D, $30, $3B, $65, $3C, $31, $35, $3B, $65, $2B, $2B, $29, $7B, $66, $6C, $6F, $61, $74, $32, $20, $66, $3D, $63, $3B, $66, $2E, $78, $2B, $3D, 
      $28, $66, $6C, $6F, $61, $74, $28, $65, $29, $2D, $37, $2E, $35, $29, $2F, $57, $69, $64, $74, $68, $3B, $64, $2B, $3D, $66, $6C, $6F, $61, $74, $28, $49, $6E, $70, $75, $74, $2E, $65, $76, $61, $6C, 
      $28, $66, $2A, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $29, $2E, $77, $29, $2A, $61, $28, $65, $29, $3B, $7D, $72, $65, $74, $75, $72, $6E, $20, $68, $61, $6C, $66, 
      $34, $28, $66, $6C, $6F, $61, $74, $34, $28, $30, $2E, $2C, $30, $2E, $2C, $30, $2E, $2C, $64, $29, $29, $3B, $7D, $0A, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Resolution', TContextShaderVariableKind.Float2, 0, 8),
      TContextShaderVariable.Create('InputResolution', TContextShaderVariableKind.Float2, 1, 8),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 2, 4)]
    ),
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FF, $FF, $FE, $FF, $29, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $7B, $00, $00, $00, $00, $02, $FF, $FF, $02, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $74, $00, $00, $00,
      $44, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $4C, $00, $00, $00, $00, $00, $00, $00, $5C, $00, $00, $00, $02, $00, $00, $00, $01, $00, $00, $00, $64, $00, $00, $00, $00, $00, $00, $00,
      $49, $6E, $70, $75, $74, $00, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $57, $69, $64, $74, $68, $00, $AB, $AB, $00, $00, $03, $00, $01, $00, $01, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65,
      $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $51, $00, $00, $05, $01, $00, $0F, $A0, $00, $00, $E0, $C0, $00, $00, $C0, $C0, $3B, $16, $45, $3D, $93, $77, $0D, $3D, $51, $00, $00, $05,
      $02, $00, $0F, $A0, $00, $00, $A0, $C0, $D8, $95, $75, $3D, $00, $00, $80, $C0, $6F, $02, $93, $3D, $51, $00, $00, $05, $03, $00, $0F, $A0, $2C, $71, $C6, $3D, $6D, $73, $CA, $3D, $00, $00, $00, $00,
      $00, $00, $00, $00, $51, $00, $00, $05, $04, $00, $0F, $A0, $00, $00, $40, $C0, $E7, $19, $A9, $3D, $00, $00, $00, $C0, $BE, $E2, $BA, $3D, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0,
      $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $01, $00, $00, $02, $00, $00, $12, $80, $00, $00, $55, $B0, $06, $00, $00, $02, $00, $00, $04, $80, $00, $00, $00, $A0, $04, $00, $00, $04,
      $00, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $55, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $01, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $01, $00, $11, $80, $00, $00, $AA, $80,
      $01, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $02, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $02, $00, $11, $80, $00, $00, $AA, $80, $02, $00, $00, $A0, $00, $00, $00, $B0,
      $01, $00, $00, $02, $03, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $03, $00, $11, $80, $00, $00, $AA, $80, $02, $00, $AA, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $04, $00, $12, $80,
      $00, $00, $55, $B0, $04, $00, $00, $04, $04, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $05, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04,
      $05, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $AA, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $06, $00, $12, $80, $00, $00, $55, $B0, $02, $00, $00, $03, $06, $00, $11, $80, $00, $00, $AA, $81,
      $00, $00, $00, $B0, $01, $00, $00, $02, $07, $00, $13, $80, $00, $00, $E4, $B0, $01, $00, $00, $02, $08, $00, $12, $80, $00, $00, $55, $B0, $02, $00, $00, $03, $08, $00, $11, $80, $00, $00, $AA, $80,
      $00, $00, $00, $B0, $42, $00, $00, $03, $09, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
      $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80,
      $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
      $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $08, $00, $0F, $80, $08, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03, $00, $00, $01, $80, $09, $00, $FF, $80,
      $01, $00, $AA, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, $01, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $FF, $80, $02, $00, $55, $A0,
      $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, $02, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $04, $00, $FF, $80, $04, $00, $55, $A0,
      $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, $04, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $06, $00, $FF, $80, $03, $00, $00, $A0,
      $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $07, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $08, $00, $FF, $80, $03, $00, $00, $A0,
      $00, $00, $00, $80, $01, $00, $00, $02, $01, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $01, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $AA, $A1, $00, $00, $00, $B0, $01, $00, $00, $02,
      $02, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $02, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $00, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $03, $00, $12, $80, $00, $00, $55, $B0,
      $04, $00, $00, $04, $03, $00, $11, $80, $00, $00, $AA, $80, $02, $00, $AA, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $04, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $04, $00, $11, $80,
      $00, $00, $AA, $80, $02, $00, $00, $A1, $00, $00, $00, $B0, $04, $00, $00, $04, $05, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $55, $A1, $00, $00, $00, $B0, $04, $00, $00, $04, $06, $00, $11, $80,
      $00, $00, $AA, $80, $01, $00, $00, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $05, $00, $12, $80, $00, $00, $55, $B0, $01, $00, $00, $02, $06, $00, $12, $80, $00, $00, $55, $B0, $42, $00, $00, $03,
      $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80,
      $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
      $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, $04, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $02, $00, $FF, $80, $04, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, $02, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $04, $00, $FF, $80, $02, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, $01, $00, $AA, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $08, $80,
      $06, $00, $FF, $80, $01, $00, $FF, $A0, $00, $00, $00, $80, $01, $00, $00, $02, $00, $00, $07, $80, $03, $00, $AA, $A0, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00], [

      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX11_level_9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $4E, $DC, $C7, $09, $45, $26, $89, $3C, $CD, $06, $B1, $CC, $30, $FB, $DA, $20, $01, $00, $00, $00, $B0, $09, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $F4, $04, $00, $00,
      $BC, $07, $00, $00, $38, $08, $00, $00, $48, $09, $00, $00, $7C, $09, $00, $00, $41, $6F, $6E, $39, $B4, $04, $00, $00, $B4, $04, $00, $00, $00, $02, $FF, $FF, $80, $04, $00, $00, $34, $00, $00, $00,
      $01, $00, $28, $00, $00, $00, $34, $00, $00, $00, $34, $00, $01, $00, $24, $00, $00, $00, $34, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $02, $FF, $FF,
      $51, $00, $00, $05, $01, $00, $0F, $A0, $00, $00, $E0, $C0, $00, $00, $C0, $C0, $3B, $16, $45, $3D, $93, $77, $0D, $3D, $51, $00, $00, $05, $02, $00, $0F, $A0, $2C, $71, $C6, $3D, $6D, $73, $CA, $3D,
      $00, $00, $00, $00, $00, $00, $00, $00, $51, $00, $00, $05, $03, $00, $0F, $A0, $00, $00, $40, $C0, $E7, $19, $A9, $3D, $00, $00, $00, $C0, $BE, $E2, $BA, $3D, $51, $00, $00, $05, $04, $00, $0F, $A0,
      $00, $00, $A0, $C0, $D8, $95, $75, $3D, $00, $00, $80, $C0, $6F, $02, $93, $3D, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0,
      $01, $00, $00, $02, $00, $00, $12, $80, $00, $00, $55, $B0, $06, $00, $00, $02, $00, $00, $04, $80, $00, $00, $00, $A0, $04, $00, $00, $04, $00, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $55, $A0,
      $00, $00, $00, $B0, $01, $00, $00, $02, $01, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $01, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02,
      $02, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $02, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $03, $00, $12, $80, $00, $00, $55, $B0,
      $04, $00, $00, $04, $03, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $AA, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $04, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $04, $00, $11, $80,
      $00, $00, $AA, $80, $03, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $05, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $05, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $AA, $A0,
      $00, $00, $00, $B0, $01, $00, $00, $02, $06, $00, $12, $80, $00, $00, $55, $B0, $02, $00, $00, $03, $06, $00, $11, $80, $00, $00, $AA, $81, $00, $00, $00, $B0, $01, $00, $00, $02, $07, $00, $13, $80,
      $00, $00, $E4, $B0, $01, $00, $00, $02, $08, $00, $12, $80, $00, $00, $55, $B0, $02, $00, $00, $03, $08, $00, $11, $80, $00, $00, $AA, $80, $00, $00, $00, $B0, $42, $00, $00, $03, $09, $00, $0F, $80,
      $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80,
      $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $08, $00, $0F, $80, $08, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03, $00, $00, $01, $80, $09, $00, $FF, $80, $01, $00, $AA, $A0, $04, $00, $00, $04, $00, $00, $01, $80,
      $01, $00, $FF, $80, $01, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $FF, $80, $04, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $03, $00, $FF, $80, $04, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $04, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $05, $00, $FF, $80, $03, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $06, $00, $FF, $80, $02, $00, $00, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $07, $00, $FF, $80, $02, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $08, $00, $FF, $80, $02, $00, $00, $A0, $00, $00, $00, $80, $01, $00, $00, $02, $01, $00, $12, $80,
      $00, $00, $55, $B0, $04, $00, $00, $04, $01, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $AA, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $02, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04,
      $02, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $00, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $03, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $03, $00, $11, $80, $00, $00, $AA, $80,
      $04, $00, $AA, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $04, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $04, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $00, $A1, $00, $00, $00, $B0,
      $04, $00, $00, $04, $05, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $55, $A1, $00, $00, $00, $B0, $04, $00, $00, $04, $06, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $00, $A1, $00, $00, $00, $B0,
      $01, $00, $00, $02, $05, $00, $12, $80, $00, $00, $55, $B0, $01, $00, $00, $02, $06, $00, $12, $80, $00, $00, $55, $B0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80,
      $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0,
      $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, $03, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80,
      $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, $04, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $04, $00, $FF, $80, $04, $00, $55, $A0, $00, $00, $00, $80,
      $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, $01, $00, $AA, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $08, $80, $06, $00, $FF, $80, $01, $00, $FF, $A0, $00, $00, $00, $80,
      $01, $00, $00, $02, $00, $00, $07, $80, $02, $00, $AA, $A0, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00, $53, $48, $44, $52, $C0, $02, $00, $00, $40, $00, $00, $00,
      $B0, $00, $00, $00, $35, $18, $00, $00, $3E, $00, $00, $00, $93, $77, $0D, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $3B, $16, $45, $3D, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $D8, $95, $75, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $6F, $02, $93, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $E7, $19, $A9, $3D,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $BE, $E2, $BA, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $2C, $71, $C6, $3D, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $6D, $73, $CA, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $2C, $71, $C6, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $BE, $E2, $BA, $3D,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $E7, $19, $A9, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $6F, $02, $93, $3D, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $D8, $95, $75, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $3B, $16, $45, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $93, $77, $0D, $3D,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $59, $00, $00, $04, $46, $8E, $20, $00, $00, $00, $00, $00, $01, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $00, $00, $00, $00,
      $58, $18, $00, $04, $00, $70, $10, $00, $00, $00, $00, $00, $55, $55, $00, $00, $62, $10, $00, $03, $32, $10, $10, $00, $00, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00, $00, $00, $00, $00,
      $68, $00, $00, $02, $03, $00, $00, $00, $36, $20, $00, $05, $22, $00, $10, $00, $00, $00, $00, $00, $1A, $10, $10, $00, $00, $00, $00, $00, $36, $00, $00, $08, $C2, $00, $10, $00, $00, $00, $00, $00,
      $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $30, $00, $00, $01, $21, $00, $00, $07, $12, $00, $10, $00, $01, $00, $00, $00, $3A, $00, $10, $00,
      $00, $00, $00, $00, $01, $40, $00, $00, $0F, $00, $00, $00, $03, $00, $04, $03, $0A, $00, $10, $00, $01, $00, $00, $00, $1E, $00, $00, $0A, $32, $00, $10, $00, $01, $00, $00, $00, $F6, $0F, $10, $00,
      $00, $00, $00, $00, $02, $40, $00, $00, $F9, $FF, $FF, $FF, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $2B, $00, $00, $05, $12, $00, $10, $00, $01, $00, $00, $00, $0A, $00, $10, $00,
      $01, $00, $00, $00, $0E, $00, $00, $08, $12, $00, $10, $00, $01, $00, $00, $00, $0A, $00, $10, $00, $01, $00, $00, $00, $0A, $80, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $20, $00, $07,
      $12, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00, $01, $00, $00, $00, $0A, $10, $10, $00, $00, $00, $00, $00, $45, $00, $00, $09, $F2, $00, $10, $00, $02, $00, $00, $00, $46, $00, $10, $00,
      $00, $00, $00, $00, $46, $7E, $10, $00, $00, $00, $00, $00, $00, $60, $10, $00, $00, $00, $00, $00, $32, $00, $00, $0A, $42, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00, $02, $00, $00, $00,
      $0A, $90, $90, $00, $3A, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $36, $00, $00, $05, $82, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00, $01, $00, $00, $00,
      $16, $00, $00, $01, $36, $00, $00, $05, $82, $20, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $36, $00, $00, $08, $72, $20, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $3E, $00, $00, $01, $53, $54, $41, $54, $74, $00, $00, $00, $10, $00, $00, $00, $03, $00, $00, $00, $0F, $00, $00, $00,
      $02, $00, $00, $00, $02, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0A, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $52, $44, $45, $46, $08, $01, $00, $00, $01, $00, $00, $00, $8C, $00, $00, $00,
      $03, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FF, $FF, $00, $11, $00, $00, $D4, $00, $00, $00, $7C, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $7C, $00, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $01, $00, $00, $00,
      $0C, $00, $00, $00, $82, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $49, $6E, $70, $75,
      $74, $00, $24, $47, $6C, $6F, $62, $61, $6C, $73, $00, $AB, $82, $00, $00, $00, $01, $00, $00, $00, $A4, $00, $00, $00, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $BC, $00, $00, $00,
      $00, $00, $00, $00, $04, $00, $00, $00, $02, $00, $00, $00, $C4, $00, $00, $00, $00, $00, $00, $00, $57, $69, $64, $74, $68, $00, $AB, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $39,
      $2E, $32, $36, $2E, $39, $35, $32, $2E, $32, $38, $34, $34, $00, $AB, $AB, $AB, $49, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $03, $03, $00, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $AB, $AB, $AB, $4F, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00,
      $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $53, $56, $5F, $54, $61, $72, $67, $65, $74, $00, $AB, $AB], [

      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 4)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.Metal'}
    {$IF defined(MACOS)}
    TContextShaderSource.Create(
      TContextShaderArch.Metal,
      TEncoding.UTF8.GetBytes(
        'using namespace metal;'+

        'struct ProjectedVertex {'+
          'float4 position [[position]];'+
          'float2 textureCoord;'+
        '};'+

        'constant int g_cKernelSize = 15;'+
        'constant float weights[15] = {'+
          '0.0345378629863262,'+
          '0.0481169037520885,'+
          '0.059957355260849,'+
          '0.0717819854617119,'+
          '0.0825689360499382,'+
          '0.091252788901329,'+
          '0.0968955457210541,'+
          '0.0988529697060585,'+
          '0.0968955457210541,'+
          '0.091252788901329,'+
          '0.0825689360499382,'+
          '0.0717819854617119,'+
          '0.059957355260849,'+
          '0.0481169037520885,'+
          '0.0345378629863262};'+

        'fragment float4 fragmentShader(const ProjectedVertex in [[stage_in]],'+
                                       'constant float4 &Width [[buffer(0)]],'+
                                       'const texture2d<float> Input [[texture(0)]],'+
                                       'const sampler InputSampler [[sampler(0)]]) {'+
          'float a = 0;'+
          'float2 newCoord = 0;'+

          'for (int i = 0; i < g_cKernelSize; i++)'+
          '{'+
            'newCoord = in.textureCoord;'+
            'newCoord.x = in.textureCoord.x + ((i - (g_cKernelSize / 2) + 0.0) / Width.x);'+
            'newCoord = clamp(newCoord, 0, 1);'+
            'a += Input.sample(InputSampler, newCoord).a * weights[i];'+
          '}'+
          'return float4(0, 0, 0, a);'+
        '}'
      ),
      [TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1),
       TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.GLSL'}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0A, $0D, $2F, $2F, $20, $67, $6C, $73, $6C, $66, $20, $6F, $75, $74, $70, $75, $74, $20, $62, $79, $20, $43,
      $67, $20, $63, $6F, $6D, $70, $69, $6C, $65, $72, $0A, $2F, $2F, $20, $63, $67, $63, $20, $76, $65, $72, $73, $69, $6F, $6E, $20, $33, $2E, $30, $2E, $30, $30, $31, $36, $2C, $20, $62, $75, $69, $6C,
      $64, $20, $64, $61, $74, $65, $20, $46, $65, $62, $20, $31, $31, $20, $32, $30, $31, $31, $0A, $2F, $2F, $20, $63, $6F, $6D, $6D, $61, $6E, $64, $20, $6C, $69, $6E, $65, $20, $61, $72, $67, $73, $3A,
      $20, $2D, $71, $20, $2D, $70, $72, $6F, $66, $69, $6C, $65, $20, $67, $6C, $73, $6C, $66, $20, $2D, $65, $6E, $74, $72, $79, $20, $6D, $61, $69, $6E, $0A, $2F, $2F, $20, $73, $6F, $75, $72, $63, $65,
      $20, $66, $69, $6C, $65, $3A, $20, $67, $6C, $6F, $77, $68, $2E, $66, $70, $73, $0A, $2F, $2F, $76, $65, $6E, $64, $6F, $72, $20, $4E, $56, $49, $44, $49, $41, $20, $43, $6F, $72, $70, $6F, $72, $61,
      $74, $69, $6F, $6E, $0A, $2F, $2F, $76, $65, $72, $73, $69, $6F, $6E, $20, $33, $2E, $30, $2E, $30, $2E, $31, $36, $0A, $2F, $2F, $70, $72, $6F, $66, $69, $6C, $65, $20, $67, $6C, $73, $6C, $66, $0A,
      $2F, $2F, $70, $72, $6F, $67, $72, $61, $6D, $20, $6D, $61, $69, $6E, $0A, $2F, $2F, $73, $65, $6D, $61, $6E, $74, $69, $63, $20, $49, $6E, $70, $75, $74, $0A, $2F, $2F, $73, $65, $6D, $61, $6E, $74,
      $69, $63, $20, $57, $69, $64, $74, $68, $0A, $2F, $2F, $76, $61, $72, $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $49, $6E, $70, $75, $74, $20, $3A, $20, $20, $3A, $20, $5F, $49, $6E, $70,
      $75, $74, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20, $66, $6C, $6F, $61, $74, $20, $57, $69, $64, $74, $68, $20, $3A, $20, $20, $3A, $20, $5F, $57, $69, $64, $74,
      $68, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20, $66, $6C, $6F, $61, $74, $32, $20, $74, $65, $78, $43, $6F, $6F, $72, $64, $20, $3A, $20, $24, $76, $69, $6E, $2E,
      $54, $45, $58, $43, $4F, $4F, $52, $44, $20, $3A, $20, $54, $45, $58, $30, $20, $3A, $20, $30, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20, $66, $6C, $6F, $61, $74, $34, $20, $6D, $61, $69,
      $6E, $20, $3A, $20, $24, $76, $6F, $75, $74, $2E, $43, $4F, $4C, $4F, $52, $20, $3A, $20, $43, $4F, $4C, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A, $0A, $76, $65, $63, $34, $20, $5F, $72, $65,
      $74, $5F, $30, $3B, $0A, $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $31, $3B, $0A, $76, $65, $63, $32, $20, $5F, $54, $4D, $50, $32, $3B, $0A, $76, $65, $63, $32, $20, $5F, $54, $4D, $50, $36, $3B,
      $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $5F, $49, $6E, $70, $75, $74, $3B, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74,
      $20, $5F, $57, $69, $64, $74, $68, $3B, $0A, $0A, $20, $2F, $2F, $20, $6D, $61, $69, $6E, $20, $70, $72, $6F, $63, $65, $64, $75, $72, $65, $2C, $20, $74, $68, $65, $20, $6F, $72, $69, $67, $69, $6E,
      $61, $6C, $20, $6E, $61, $6D, $65, $20, $77, $61, $73, $20, $6D, $61, $69, $6E, $0A, $76, $6F, $69, $64, $20, $6D, $61, $69, $6E, $28, $29, $0A, $7B, $0A, $0A, $20, $20, $20, $20, $66, $6C, $6F, $61,
      $74, $20, $5F, $61, $3B, $0A, $20, $20, $20, $20, $76, $65, $63, $32, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $3B, $0A, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64,
      $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $2D,
      $37, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28,
      $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C,
      $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30,
      $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20,
      $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20,
      $5F, $61, $20, $3D, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $33, $2E, $34, $35, $33, $37, $38, $36, $33, $30, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20,
      $2B, $20, $2D, $36, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D,
      $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30,
      $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E,
      $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B,
      $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20,
      $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $34, $2E, $38, $31, $31, $36, $39, $30, $33, $38, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20,
      $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D,
      $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $2D, $35, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F,
      $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30,
      $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28,
      $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C,
      $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F,
      $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $35, $2E, $39, $39, $35, $37, $33, $35, $35, $33, $45,
      $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43,
      $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $2D, $34, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74,
      $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30,
      $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50,
      $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30,
      $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F,
      $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $37, $2E, $31,
      $37, $38, $31, $39, $38, $35, $35, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20,
      $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $2D, $33, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30,
      $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30,
      $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A,
      $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30,
      $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78,
      $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D,
      $50, $31, $2E, $77, $2A, $38, $2E, $32, $35, $36, $38, $39, $33, $36, $30, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45,
      $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $2D, $32, $2E, $30, $30, $30,
      $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28,
      $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77,
      $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30,
      $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D,
      $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20,
      $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $31, $32, $35, $32, $37, $38, $38, $39, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20,
      $2B, $20, $2D, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D,
      $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30,
      $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E,
      $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B,
      $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20,
      $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $36, $38, $39, $35, $35, $34, $35, $37, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20,
      $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D,
      $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54,
      $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30,
      $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76,
      $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20,
      $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54,
      $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $38, $38, $35, $32, $39, $36, $39, $37, $45, $2D,
      $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B,
      $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20,
      $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20,
      $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45,
      $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E,
      $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $36, $38, $39,
      $35, $35, $34, $35, $37, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20,
      $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $32, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F,
      $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30,
      $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20,
      $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30,
      $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72,
      $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E,
      $77, $2A, $39, $2E, $31, $32, $35, $32, $37, $38, $38, $39, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E,
      $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $33, $2E, $30, $30, $30, $30, $30, $30, $30,
      $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30,
      $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72,
      $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30,
      $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D,
      $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B,
      $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $38, $2E, $32, $35, $36, $38, $39, $33, $36, $30, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20,
      $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $34, $2E,
      $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65,
      $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F,
      $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30,
      $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20,
      $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61,
      $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $37, $2E, $31, $37, $38, $31, $39, $38, $35, $35, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65,
      $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30,
      $2E, $78, $20, $2B, $20, $35, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D,
      $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B,
      $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20,
      $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32,
      $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B,
      $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $35, $2E, $39, $39, $35, $37, $33, $35, $35, $33, $45, $2D, $30, $30, $32, $3B, $0A,
      $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78,
      $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $36, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20,
      $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30,
      $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78,
      $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29,
      $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20,
      $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $34, $2E, $38, $31, $31, $36, $39, $30, $33, $38,
      $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77,
      $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $37, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74,
      $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30,
      $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50,
      $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30,
      $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F,
      $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $33, $2E, $34,
      $35, $33, $37, $38, $36, $33, $30, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $72, $65, $74, $5F, $30, $20, $3D, $20, $76, $65, $63, $34, $28, $30, $2E, $30, $30, $30, $30, $30, $30,
      $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30,
      $30, $2C, $20, $5F, $61, $29, $3B, $0A, $20, $20, $20, $20, $67, $6C, $5F, $46, $72, $61, $67, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $72, $65, $74, $5F, $30, $3B, $0A, $20, $20, $20, $20, $72,
      $65, $74, $75, $72, $6E, $3B, $0A, $7D, $20, $2F, $2F, $20, $6D, $61, $69, $6E, $20, $65, $6E, $64, $0A], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1)]
    )
    {$ENDREGION}

  ]);
  FShaders[1] := TShaderManager.RegisterShaderFromData('glowv.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.SKSL'}
    TContextShaderSource.Create(TContextShaderArch.SKSL, [
      $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $68, $61, $64, $65, $72, 
      $20, $49, $6E, $70, $75, $74, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, 
      $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $57, $69, $64, $74, $68, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $34, $20, $46, $69, $6C, $6C, $43, $6F, $6C, 
      $6F, $72, $3B, $69, $6E, $6C, $69, $6E, $65, $20, $66, $6C, $6F, $61, $74, $20, $61, $28, $63, $6F, $6E, $73, $74, $20, $69, $6E, $74, $20, $62, $29, $7B, $73, $77, $69, $74, $63, $68, $28, $62, $29, 
      $7B, $63, $61, $73, $65, $20, $30, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $33, $34, $35, $33, $37, $38, $36, $33, $3B, $63, $61, $73, $65, $20, $31, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, 
      $34, $38, $31, $31, $36, $39, $30, $33, $38, $3B, $63, $61, $73, $65, $20, $32, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $35, $39, $39, $35, $37, $33, $35, $35, $33, $3B, $63, $61, $73, $65, $20, 
      $33, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $37, $31, $37, $38, $31, $39, $38, $35, $35, $3B, $63, $61, $73, $65, $20, $34, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $38, $32, $35, $36, $38, 
      $39, $33, $36, $3B, $63, $61, $73, $65, $20, $35, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $39, $31, $32, $35, $32, $37, $39, $3B, $63, $61, $73, $65, $20, $36, $3A, $72, $65, $74, $75, $72, $6E, 
      $2E, $30, $39, $36, $38, $39, $35, $35, $34, $35, $37, $3B, $63, $61, $73, $65, $20, $37, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $39, $38, $38, $35, $32, $39, $37, $3B, $63, $61, $73, $65, $20, 
      $38, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $39, $36, $38, $39, $35, $35, $34, $35, $37, $3B, $63, $61, $73, $65, $20, $39, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $39, $31, $32, $35, $32, 
      $37, $39, $3B, $63, $61, $73, $65, $20, $31, $30, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $38, $32, $35, $36, $38, $39, $33, $36, $3B, $63, $61, $73, $65, $20, $31, $31, $3A, $72, $65, $74, $75, 
      $72, $6E, $2E, $30, $37, $31, $37, $38, $31, $39, $38, $35, $35, $3B, $63, $61, $73, $65, $20, $31, $32, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $35, $39, $39, $35, $37, $33, $35, $35, $33, $3B, 
      $63, $61, $73, $65, $20, $31, $33, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $34, $38, $31, $31, $36, $39, $30, $33, $38, $3B, $64, $65, $66, $61, $75, $6C, $74, $3A, $72, $65, $74, $75, $72, $6E, 
      $2E, $30, $33, $34, $35, $33, $37, $38, $36, $33, $3B, $7D, $7D, $68, $61, $6C, $66, $34, $20, $6D, $61, $69, $6E, $28, $66, $6C, $6F, $61, $74, $32, $20, $62, $29, $7B, $66, $6C, $6F, $61, $74, $32, 
      $20, $63, $3D, $62, $2F, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $66, $6C, $6F, $61, $74, $20, $64, $3D, $30, $2E, $3B, $66, $6F, $72, $28, $69, $6E, $74, $20, $65, $3D, $30, $3B, $65, 
      $3C, $31, $35, $3B, $65, $2B, $2B, $29, $7B, $66, $6C, $6F, $61, $74, $32, $20, $66, $3D, $63, $3B, $66, $2E, $79, $2B, $3D, $28, $66, $6C, $6F, $61, $74, $28, $65, $29, $2D, $37, $2E, $35, $29, $2F, 
      $57, $69, $64, $74, $68, $3B, $64, $2B, $3D, $66, $6C, $6F, $61, $74, $28, $49, $6E, $70, $75, $74, $2E, $65, $76, $61, $6C, $28, $66, $2A, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, 
      $69, $6F, $6E, $29, $2E, $77, $29, $2A, $61, $28, $65, $29, $3B, $7D, $72, $65, $74, $75, $72, $6E, $20, $68, $61, $6C, $66, $34, $28, $46, $69, $6C, $6C, $43, $6F, $6C, $6F, $72, $2A, $64, $29, $3B, 
      $7D, $0A, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Resolution', TContextShaderVariableKind.Float2, 0, 8),
      TContextShaderVariable.Create('InputResolution', TContextShaderVariableKind.Float2, 1, 8),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 2, 4),
      TContextShaderVariable.Create('FillColor', TContextShaderVariableKind.Vector, 3, 16)]
    ),
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FF, $FF, $FE, $FF, $35, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $AB, $00, $00, $00, $00, $02, $FF, $FF, $03, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $A4, $00, $00, $00,
      $58, $00, $00, $00, $02, $00, $01, $00, $01, $00, $00, $00, $64, $00, $00, $00, $00, $00, $00, $00, $74, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $7C, $00, $00, $00, $00, $00, $00, $00,
      $8C, $00, $00, $00, $02, $00, $00, $00, $01, $00, $00, $00, $94, $00, $00, $00, $00, $00, $00, $00, $46, $69, $6C, $6C, $43, $6F, $6C, $6F, $72, $00, $AB, $AB, $01, $00, $03, $00, $01, $00, $04, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $49, $6E, $70, $75, $74, $00, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $57, $69, $64, $74, $68, $00, $AB, $AB,
      $00, $00, $03, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44,
      $58, $39, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $51, $00, $00, $05, $02, $00, $0F, $A0, $00, $00, $E0, $C0, $00, $00, $C0, $C0, $3B, $16, $45, $3D,
      $93, $77, $0D, $3D, $51, $00, $00, $05, $03, $00, $0F, $A0, $00, $00, $A0, $C0, $D8, $95, $75, $3D, $00, $00, $80, $C0, $6F, $02, $93, $3D, $51, $00, $00, $05, $04, $00, $0F, $A0, $2C, $71, $C6, $3D,
      $6D, $73, $CA, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $51, $00, $00, $05, $05, $00, $0F, $A0, $00, $00, $40, $C0, $E7, $19, $A9, $3D, $00, $00, $00, $C0, $BE, $E2, $BA, $3D, $1F, $00, $00, $02,
      $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $01, $00, $00, $02, $00, $00, $11, $80, $00, $00, $00, $B0, $06, $00, $00, $02, $00, $00, $04, $80,
      $00, $00, $00, $A0, $04, $00, $00, $04, $00, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $55, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $01, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04,
      $01, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $02, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $02, $00, $12, $80, $00, $00, $AA, $80,
      $03, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $03, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $03, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $AA, $A0, $00, $00, $55, $B0,
      $01, $00, $00, $02, $04, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $04, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $05, $00, $11, $80,
      $00, $00, $00, $B0, $04, $00, $00, $04, $05, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $AA, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $06, $00, $11, $80, $00, $00, $00, $B0, $02, $00, $00, $03,
      $06, $00, $12, $80, $00, $00, $AA, $81, $00, $00, $55, $B0, $01, $00, $00, $02, $07, $00, $13, $80, $00, $00, $E4, $B0, $01, $00, $00, $02, $08, $00, $11, $80, $00, $00, $00, $B0, $02, $00, $00, $03,
      $08, $00, $12, $80, $00, $00, $AA, $80, $00, $00, $55, $B0, $42, $00, $00, $03, $09, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80,
      $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
      $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80,
      $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $08, $00, $0F, $80, $08, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03,
      $00, $00, $01, $80, $09, $00, $FF, $80, $02, $00, $AA, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, $02, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $02, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, $03, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $04, $00, $FF, $80, $05, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, $05, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $06, $00, $FF, $80, $04, $00, $00, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $07, $00, $FF, $80, $04, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $08, $00, $FF, $80, $04, $00, $00, $A0, $00, $00, $00, $80, $01, $00, $00, $02, $01, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $01, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $AA, $A1,
      $00, $00, $55, $B0, $01, $00, $00, $02, $02, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $02, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $00, $A1, $00, $00, $55, $B0, $01, $00, $00, $02,
      $03, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $03, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $AA, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $04, $00, $11, $80, $00, $00, $00, $B0,
      $04, $00, $00, $04, $04, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $00, $A1, $00, $00, $55, $B0, $04, $00, $00, $04, $05, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $55, $A1, $00, $00, $55, $B0,
      $04, $00, $00, $04, $06, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $00, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $05, $00, $11, $80, $00, $00, $00, $B0, $01, $00, $00, $02, $06, $00, $11, $80,
      $00, $00, $00, $B0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
      $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80,
      $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, $05, $00, $FF, $A0, $00, $00, $00, $80,
      $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $FF, $80, $05, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, $03, $00, $FF, $A0, $00, $00, $00, $80,
      $04, $00, $00, $04, $00, $00, $01, $80, $04, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, $02, $00, $AA, $A0, $00, $00, $00, $80,
      $04, $00, $00, $04, $00, $00, $01, $80, $06, $00, $FF, $80, $02, $00, $FF, $A0, $00, $00, $00, $80, $05, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $00, $80, $01, $00, $E4, $A0, $01, $00, $00, $02,
      $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('FillColor', TContextShaderVariableKind.Vector, 1, 1),
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX11_level_9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $07, $E9, $AB, $3B, $64, $46, $22, $F3, $E4, $BE, $AF, $AD, $0F, $A5, $93, $09, $01, $00, $00, $00, $D4, $09, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $F8, $04, $00, $00,
      $AC, $07, $00, $00, $28, $08, $00, $00, $6C, $09, $00, $00, $A0, $09, $00, $00, $41, $6F, $6E, $39, $B8, $04, $00, $00, $B8, $04, $00, $00, $00, $02, $FF, $FF, $84, $04, $00, $00, $34, $00, $00, $00,
      $01, $00, $28, $00, $00, $00, $34, $00, $00, $00, $34, $00, $01, $00, $24, $00, $00, $00, $34, $00, $00, $00, $00, $00, $00, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00, $00, $02, $FF, $FF,
      $51, $00, $00, $05, $02, $00, $0F, $A0, $00, $00, $E0, $C0, $00, $00, $C0, $C0, $3B, $16, $45, $3D, $93, $77, $0D, $3D, $51, $00, $00, $05, $03, $00, $0F, $A0, $00, $00, $A0, $C0, $D8, $95, $75, $3D,
      $00, $00, $80, $C0, $6F, $02, $93, $3D, $51, $00, $00, $05, $04, $00, $0F, $A0, $2C, $71, $C6, $3D, $6D, $73, $CA, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $51, $00, $00, $05, $05, $00, $0F, $A0,
      $00, $00, $40, $C0, $E7, $19, $A9, $3D, $00, $00, $00, $C0, $BE, $E2, $BA, $3D, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0,
      $01, $00, $00, $02, $00, $00, $11, $80, $00, $00, $00, $B0, $06, $00, $00, $02, $00, $00, $04, $80, $00, $00, $00, $A0, $04, $00, $00, $04, $00, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $55, $A0,
      $00, $00, $55, $B0, $01, $00, $00, $02, $01, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $01, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02,
      $02, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $02, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $03, $00, $11, $80, $00, $00, $00, $B0,
      $04, $00, $00, $04, $03, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $AA, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $04, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $04, $00, $12, $80,
      $00, $00, $AA, $80, $05, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $05, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $05, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $AA, $A0,
      $00, $00, $55, $B0, $01, $00, $00, $02, $06, $00, $11, $80, $00, $00, $00, $B0, $02, $00, $00, $03, $06, $00, $12, $80, $00, $00, $AA, $81, $00, $00, $55, $B0, $01, $00, $00, $02, $07, $00, $13, $80,
      $00, $00, $E4, $B0, $01, $00, $00, $02, $08, $00, $11, $80, $00, $00, $00, $B0, $02, $00, $00, $03, $08, $00, $12, $80, $00, $00, $AA, $80, $00, $00, $55, $B0, $42, $00, $00, $03, $09, $00, $0F, $80,
      $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80,
      $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $08, $00, $0F, $80, $08, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03, $00, $00, $01, $80, $09, $00, $FF, $80, $02, $00, $AA, $A0, $04, $00, $00, $04, $00, $00, $01, $80,
      $01, $00, $FF, $80, $02, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $03, $00, $FF, $80, $03, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $04, $00, $FF, $80, $05, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $05, $00, $FF, $80, $05, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $06, $00, $FF, $80, $04, $00, $00, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $07, $00, $FF, $80, $04, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $08, $00, $FF, $80, $04, $00, $00, $A0, $00, $00, $00, $80, $01, $00, $00, $02, $01, $00, $11, $80,
      $00, $00, $00, $B0, $04, $00, $00, $04, $01, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $AA, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $02, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04,
      $02, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $00, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $03, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $03, $00, $12, $80, $00, $00, $AA, $80,
      $03, $00, $AA, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $04, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $04, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $00, $A1, $00, $00, $55, $B0,
      $04, $00, $00, $04, $05, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $55, $A1, $00, $00, $55, $B0, $04, $00, $00, $04, $06, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $00, $A1, $00, $00, $55, $B0,
      $01, $00, $00, $02, $05, $00, $11, $80, $00, $00, $00, $B0, $01, $00, $00, $02, $06, $00, $11, $80, $00, $00, $00, $B0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80,
      $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0,
      $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, $05, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $FF, $80, $05, $00, $55, $A0, $00, $00, $00, $80,
      $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, $03, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $04, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80,
      $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, $02, $00, $AA, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $06, $00, $FF, $80, $02, $00, $FF, $A0, $00, $00, $00, $80,
      $05, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $00, $80, $01, $00, $E4, $A0, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00, $53, $48, $44, $52, $AC, $02, $00, $00,
      $40, $00, $00, $00, $AB, $00, $00, $00, $35, $18, $00, $00, $3E, $00, $00, $00, $93, $77, $0D, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $3B, $16, $45, $3D, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $D8, $95, $75, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $6F, $02, $93, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $E7, $19, $A9, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $BE, $E2, $BA, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $2C, $71, $C6, $3D, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $6D, $73, $CA, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $2C, $71, $C6, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $BE, $E2, $BA, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $E7, $19, $A9, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $6F, $02, $93, $3D, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $D8, $95, $75, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $3B, $16, $45, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $93, $77, $0D, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $59, $00, $00, $04, $46, $8E, $20, $00, $00, $00, $00, $00, $02, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00,
      $00, $00, $00, $00, $58, $18, $00, $04, $00, $70, $10, $00, $00, $00, $00, $00, $55, $55, $00, $00, $62, $10, $00, $03, $32, $10, $10, $00, $00, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00,
      $00, $00, $00, $00, $68, $00, $00, $02, $03, $00, $00, $00, $36, $20, $00, $05, $12, $00, $10, $00, $00, $00, $00, $00, $0A, $10, $10, $00, $00, $00, $00, $00, $36, $00, $00, $08, $C2, $00, $10, $00,
      $00, $00, $00, $00, $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $30, $00, $00, $01, $21, $00, $00, $07, $12, $00, $10, $00, $01, $00, $00, $00,
      $3A, $00, $10, $00, $00, $00, $00, $00, $01, $40, $00, $00, $0F, $00, $00, $00, $03, $00, $04, $03, $0A, $00, $10, $00, $01, $00, $00, $00, $1E, $00, $00, $0A, $32, $00, $10, $00, $01, $00, $00, $00,
      $F6, $0F, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00, $F9, $FF, $FF, $FF, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $2B, $00, $00, $05, $12, $00, $10, $00, $01, $00, $00, $00,
      $0A, $00, $10, $00, $01, $00, $00, $00, $0E, $00, $00, $08, $12, $00, $10, $00, $01, $00, $00, $00, $0A, $00, $10, $00, $01, $00, $00, $00, $0A, $80, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $20, $00, $07, $22, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00, $01, $00, $00, $00, $1A, $10, $10, $00, $00, $00, $00, $00, $45, $00, $00, $09, $F2, $00, $10, $00, $02, $00, $00, $00,
      $46, $00, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00, $00, $00, $00, $00, $00, $60, $10, $00, $00, $00, $00, $00, $32, $00, $00, $0A, $42, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00,
      $02, $00, $00, $00, $0A, $90, $90, $00, $3A, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $36, $00, $00, $05, $82, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00,
      $01, $00, $00, $00, $16, $00, $00, $01, $38, $00, $00, $08, $F2, $20, $10, $00, $00, $00, $00, $00, $A6, $0A, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $01, $00, $00, $00,
      $3E, $00, $00, $01, $53, $54, $41, $54, $74, $00, $00, $00, $0F, $00, $00, $00, $03, $00, $00, $00, $0F, $00, $00, $00, $02, $00, $00, $00, $03, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00,
      $01, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $09, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $52, $44, $45, $46, $3C, $01, $00, $00, $01, $00, $00, $00, $8C, $00, $00, $00, $03, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FF, $FF, $00, $11, $00, $00,
      $08, $01, $00, $00, $7C, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $7C, $00, $00, $00,
      $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $01, $00, $00, $00, $0C, $00, $00, $00, $82, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $49, $6E, $70, $75, $74, $00, $24, $47, $6C, $6F, $62, $61, $6C, $73, $00, $AB, $82, $00, $00, $00,
      $02, $00, $00, $00, $A4, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $D4, $00, $00, $00, $00, $00, $00, $00, $04, $00, $00, $00, $02, $00, $00, $00, $DC, $00, $00, $00,
      $00, $00, $00, $00, $EC, $00, $00, $00, $10, $00, $00, $00, $10, $00, $00, $00, $02, $00, $00, $00, $F8, $00, $00, $00, $00, $00, $00, $00, $57, $69, $64, $74, $68, $00, $AB, $AB, $00, $00, $03, $00,
      $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $46, $69, $6C, $6C, $43, $6F, $6C, $6F, $72, $00, $AB, $AB, $01, $00, $03, $00, $01, $00, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E,
      $39, $35, $32, $2E, $32, $38, $34, $34, $00, $AB, $AB, $AB, $49, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $03, $00, $00, $00, $00, $00, $00, $00, $03, $03, $00, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $AB, $AB, $AB, $4F, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00,
      $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $53, $56, $5F, $54, $61, $72, $67, $65, $74, $00, $AB, $AB], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 4),
      TContextShaderVariable.Create('FillColor', TContextShaderVariableKind.Vector, 16, 16)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.Metal'}
    {$IF defined(MACOS)}
    TContextShaderSource.Create(
      TContextShaderArch.Metal,
      TEncoding.UTF8.GetBytes(
        'using namespace metal;'+

        'struct ProjectedVertex {'+
          'float4 position [[position]];'+
          'float2 textureCoord;'+
        '};'+

        'constant int g_cKernelSize = 15;'+
        'constant float weights[15] = {'+
          '0.0345378629863262,'+
          '0.0481169037520885,'+
          '0.059957355260849,'+
          '0.0717819854617119,'+
          '0.0825689360499382,'+
          '0.091252788901329,'+
          '0.0968955457210541,'+
          '0.0988529697060585,'+
          '0.0968955457210541,'+
          '0.091252788901329,'+
          '0.0825689360499382,'+
          '0.0717819854617119,'+
          '0.059957355260849,'+
          '0.0481169037520885,'+
          '0.0345378629863262};'+

        'fragment float4 fragmentShader(const ProjectedVertex in [[stage_in]],'+
                                       'constant float4 &Width [[buffer(0)]],'+
                                       'constant float4 &FillColor [[buffer(1)]],'+
                                       'const texture2d<float> Input [[texture(0)]],'+
                                       'const sampler InputSampler [[sampler(0)]]) {'+
          'float a = 0;'+
          'float2 newCoord = 0;'+

          'for (int i = 0; i < g_cKernelSize; i++)'+
          '{'+
            'newCoord = in.textureCoord;'+
            'newCoord.y = in.textureCoord.y + ((i - (g_cKernelSize / 2) + 0.0) / Width.x);'+
            'newCoord = clamp(newCoord, 0, 1);'+
            'a += Input.sample(InputSampler, newCoord).a * weights[i];'+
          '}'+
          'return FillColor * a;'+
        '}'
      ),
      [TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1),
       TContextShaderVariable.Create('FillColor', TContextShaderVariableKind.Vector, 1, 1),
       TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.GLSL'}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0A, $0D, $2F, $2F, $20, $67, $6C, $73, $6C, $66, $20, $6F, $75, $74, $70, $75, $74, $20, $62, $79, $20, $43,
      $67, $20, $63, $6F, $6D, $70, $69, $6C, $65, $72, $0A, $2F, $2F, $20, $63, $67, $63, $20, $76, $65, $72, $73, $69, $6F, $6E, $20, $33, $2E, $30, $2E, $30, $30, $31, $36, $2C, $20, $62, $75, $69, $6C,
      $64, $20, $64, $61, $74, $65, $20, $46, $65, $62, $20, $31, $31, $20, $32, $30, $31, $31, $0A, $2F, $2F, $20, $63, $6F, $6D, $6D, $61, $6E, $64, $20, $6C, $69, $6E, $65, $20, $61, $72, $67, $73, $3A,
      $20, $2D, $71, $20, $2D, $70, $72, $6F, $66, $69, $6C, $65, $20, $67, $6C, $73, $6C, $66, $20, $2D, $65, $6E, $74, $72, $79, $20, $6D, $61, $69, $6E, $0A, $2F, $2F, $20, $73, $6F, $75, $72, $63, $65,
      $20, $66, $69, $6C, $65, $3A, $20, $67, $6C, $6F, $77, $76, $2E, $66, $70, $73, $0A, $2F, $2F, $76, $65, $6E, $64, $6F, $72, $20, $4E, $56, $49, $44, $49, $41, $20, $43, $6F, $72, $70, $6F, $72, $61,
      $74, $69, $6F, $6E, $0A, $2F, $2F, $76, $65, $72, $73, $69, $6F, $6E, $20, $33, $2E, $30, $2E, $30, $2E, $31, $36, $0A, $2F, $2F, $70, $72, $6F, $66, $69, $6C, $65, $20, $67, $6C, $73, $6C, $66, $0A,
      $2F, $2F, $70, $72, $6F, $67, $72, $61, $6D, $20, $6D, $61, $69, $6E, $0A, $2F, $2F, $73, $65, $6D, $61, $6E, $74, $69, $63, $20, $49, $6E, $70, $75, $74, $0A, $2F, $2F, $73, $65, $6D, $61, $6E, $74,
      $69, $63, $20, $57, $69, $64, $74, $68, $0A, $2F, $2F, $73, $65, $6D, $61, $6E, $74, $69, $63, $20, $46, $69, $6C, $6C, $43, $6F, $6C, $6F, $72, $0A, $2F, $2F, $76, $61, $72, $20, $73, $61, $6D, $70,
      $6C, $65, $72, $32, $44, $20, $49, $6E, $70, $75, $74, $20, $3A, $20, $20, $3A, $20, $5F, $49, $6E, $70, $75, $74, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20, $66,
      $6C, $6F, $61, $74, $20, $57, $69, $64, $74, $68, $20, $3A, $20, $20, $3A, $20, $5F, $57, $69, $64, $74, $68, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20, $66, $6C,
      $6F, $61, $74, $34, $20, $46, $69, $6C, $6C, $43, $6F, $6C, $6F, $72, $20, $3A, $20, $20, $3A, $20, $5F, $46, $69, $6C, $6C, $43, $6F, $6C, $6F, $72, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A,
      $2F, $2F, $76, $61, $72, $20, $66, $6C, $6F, $61, $74, $32, $20, $74, $65, $78, $43, $6F, $6F, $72, $64, $20, $3A, $20, $24, $76, $69, $6E, $2E, $54, $45, $58, $43, $4F, $4F, $52, $44, $20, $3A, $20,
      $54, $45, $58, $30, $20, $3A, $20, $30, $20, $3A, $20, $31, $0A, $2F, $2F, $76, $61, $72, $20, $66, $6C, $6F, $61, $74, $34, $20, $6D, $61, $69, $6E, $20, $3A, $20, $24, $76, $6F, $75, $74, $2E, $43,
      $4F, $4C, $4F, $52, $20, $3A, $20, $43, $4F, $4C, $20, $3A, $20, $2D, $31, $20, $3A, $20, $31, $0A, $0A, $76, $65, $63, $34, $20, $5F, $72, $65, $74, $5F, $30, $3B, $0A, $76, $65, $63, $34, $20, $5F,
      $54, $4D, $50, $31, $3B, $0A, $76, $65, $63, $32, $20, $5F, $54, $4D, $50, $32, $3B, $0A, $76, $65, $63, $32, $20, $5F, $54, $4D, $50, $37, $3B, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $61,
      $6D, $70, $6C, $65, $72, $32, $44, $20, $5F, $49, $6E, $70, $75, $74, $3B, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $5F, $57, $69, $64, $74, $68, $3B, $0A, $75, $6E,
      $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $34, $20, $5F, $46, $69, $6C, $6C, $43, $6F, $6C, $6F, $72, $3B, $0A, $0A, $20, $2F, $2F, $20, $6D, $61, $69, $6E, $20, $70, $72, $6F, $63, $65, $64, $75,
      $72, $65, $2C, $20, $74, $68, $65, $20, $6F, $72, $69, $67, $69, $6E, $61, $6C, $20, $6E, $61, $6D, $65, $20, $77, $61, $73, $20, $6D, $61, $69, $6E, $0A, $76, $6F, $69, $64, $20, $6D, $61, $69, $6E,
      $28, $29, $0A, $7B, $0A, $0A, $20, $20, $20, $20, $66, $6C, $6F, $61, $74, $20, $5F, $61, $3B, $0A, $20, $20, $20, $20, $76, $65, $63, $32, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $3B, $0A,
      $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E,
      $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $37, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20,
      $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30,
      $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $6D,
      $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30,
      $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74,
      $2C, $20, $5F, $54, $4D, $50, $37, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $33, $2E, $34, $35, $33, $37, $38, $36, $33, $30, $45, $2D, $30,
      $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F,
      $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $36, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B,
      $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20,
      $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $20,
      $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45,
      $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E,
      $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $37, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $34, $2E, $38, $31, $31,
      $36, $39, $30, $33, $38, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20,
      $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $35, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30,
      $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30,
      $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20,
      $20, $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30,
      $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75,
      $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $37, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31,
      $2E, $77, $2A, $35, $2E, $39, $39, $35, $37, $33, $35, $35, $33, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30,
      $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $34, $2E, $30, $30, $30, $30, $30,
      $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31,
      $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B,
      $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31,
      $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $37, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61,
      $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $37, $2E, $31, $37, $38, $31, $39, $38, $35, $35, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72,
      $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20,
      $2D, $33, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E,
      $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29,
      $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30,
      $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20,
      $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $37, $29, $3B, $0A, $20, $20, $20,
      $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $38, $2E, $32, $35, $36, $38, $39, $33, $36, $30, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20,
      $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54,
      $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $32, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D,
      $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30,
      $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $6D, $61, $78, $28, $76, $65,
      $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F,
      $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D,
      $50, $37, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $31, $32, $35, $32, $37, $38, $38, $39, $45, $2D, $30,
      $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F,
      $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B,
      $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20,
      $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $20,
      $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45,
      $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E,
      $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $37, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $36, $38, $39,
      $35, $35, $34, $35, $37, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20,
      $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F,
      $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30,
      $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20,
      $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30,
      $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72,
      $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $37, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E,
      $77, $2A, $39, $2E, $38, $38, $35, $32, $39, $36, $39, $37, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E,
      $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30,
      $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30,
      $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72,
      $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30,
      $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D,
      $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $37, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B,
      $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $36, $38, $39, $35, $35, $34, $35, $37, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20,
      $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $32, $2E,
      $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65,
      $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F,
      $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30,
      $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20,
      $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $37, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61,
      $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $31, $32, $35, $32, $37, $38, $38, $39, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65,
      $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30,
      $2E, $79, $20, $2B, $20, $33, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D,
      $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B,
      $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20,
      $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32,
      $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $37, $29, $3B,
      $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $38, $2E, $32, $35, $36, $38, $39, $33, $36, $30, $45, $2D, $30, $30, $32, $3B, $0A,
      $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79,
      $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $34, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20,
      $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30,
      $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $6D, $61, $78,
      $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29,
      $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20,
      $5F, $54, $4D, $50, $37, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $37, $2E, $31, $37, $38, $31, $39, $38, $35, $35,
      $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77,
      $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $35, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74,
      $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30,
      $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50,
      $37, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30,
      $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F,
      $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $37, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $35, $2E, $39,
      $39, $35, $37, $33, $35, $35, $33, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0A, $20,
      $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $36, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30,
      $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $30, $30, $30, $30, $30,
      $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0A, $20,
      $20, $20, $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $30, $2E,
      $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74,
      $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $37, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50,
      $31, $2E, $77, $2A, $34, $2E, $38, $31, $31, $36, $39, $30, $33, $38, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58,
      $30, $2E, $78, $79, $3B, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $37, $2E, $30, $30, $30, $30, $30,
      $30, $30, $30, $45, $2B, $30, $30, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31,
      $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $2C, $20, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B,
      $30, $30, $30, $2C, $20, $30, $2E, $30, $30, $30, $30, $30, $30, $30, $30, $45, $2B, $30, $30, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31,
      $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $37, $29, $3B, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61,
      $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $33, $2E, $34, $35, $33, $37, $38, $36, $33, $30, $45, $2D, $30, $30, $32, $3B, $0A, $20, $20, $20, $20, $5F, $72, $65, $74, $5F, $30, $20, $3D,
      $20, $5F, $46, $69, $6C, $6C, $43, $6F, $6C, $6F, $72, $2A, $5F, $61, $3B, $0A, $20, $20, $20, $20, $67, $6C, $5F, $46, $72, $61, $67, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $72, $65, $74, $5F,
      $30, $3B, $0A, $20, $20, $20, $20, $72, $65, $74, $75, $72, $6E, $3B, $0A, $7D, $20, $2F, $2F, $20, $6D, $61, $69, $6E, $20, $65, $6E, $64, $0A], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1),
      TContextShaderVariable.Create('FillColor', TContextShaderVariableKind.Vector, 0, 1)]
    )
    {$ENDREGION}

  ]);
  FPassCount := 2;
end;

destructor TGlowFilter.Destroy;
begin
  inherited;
end;

procedure TGlowFilter.LoadShaders;
const
  Scale = 1.5;
var
  C: TAlphaColorRec;
begin
  FilterContext.SetShaders(FVertexShader, FShaders[FPass]);
  if FPass = 0 then
    FilterContext.SetShaderVariable('Width', [Vector3D(InputSize.Width / ValuesAsFloat['BlurAmount'] / Scale, 0, 0, 0)]);
  if FPass = 1 then
  begin
    FilterContext.SetShaderVariable('Width', [Vector3D(InputSize.Height / ValuesAsFloat['BlurAmount'] / Scale, 0, 0, 0)]);
    C := TAlphaColorRec(ValuesAsColor['Color']);
    FilterContext.SetShaderVariable('FillColor', [Vector3D(C.R / $FF, C.G / $FF, C.B / $FF, C.A / $FF)]);
  end;
end;

class function TGlowFilter.FilterAttr: TFilterRec;
begin
  Result := TFilterRec.Create('GlowFilter', 'An effect that add glow.', [
    TFilterValueRec.Create('BlurAmount', 'The blur factor.', TFilterValueType.Float, 0.7, 0.01, 10),
    TFilterValueRec.Create('Color', 'The glow color.', TFilterValueType.Color, $FFFFD700, 0, 0)
  ]);
end;

{ TInnerGlowFilter }

constructor TInnerGlowFilter.Create;
begin
  inherited;
  FShaders[0] := TShaderManager.RegisterShaderFromData('innerglowh.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.SKSL'}
    TContextShaderSource.Create(TContextShaderArch.SKSL, [
      $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $68, $61, $64, $65, $72, 
      $20, $49, $6E, $70, $75, $74, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, 
      $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $57, $69, $64, $74, $68, $3B, $69, $6E, $6C, $69, $6E, $65, $20, $66, $6C, $6F, $61, $74, $20, $61, $28, $63, $6F, $6E, $73, $74, $20, $69, 
      $6E, $74, $20, $62, $29, $7B, $73, $77, $69, $74, $63, $68, $28, $62, $29, $7B, $63, $61, $73, $65, $20, $30, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $33, $34, $35, $33, $37, $38, $36, $33, $3B, 
      $63, $61, $73, $65, $20, $31, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $34, $38, $31, $31, $36, $39, $30, $33, $38, $3B, $63, $61, $73, $65, $20, $32, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, 
      $35, $39, $39, $35, $37, $33, $35, $35, $33, $3B, $63, $61, $73, $65, $20, $33, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $37, $31, $37, $38, $31, $39, $38, $35, $35, $3B, $63, $61, $73, $65, $20, 
      $34, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $38, $32, $35, $36, $38, $39, $33, $36, $3B, $63, $61, $73, $65, $20, $35, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $39, $31, $32, $35, $32, $37, 
      $39, $3B, $63, $61, $73, $65, $20, $36, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $39, $36, $38, $39, $35, $35, $34, $35, $37, $3B, $63, $61, $73, $65, $20, $37, $3A, $72, $65, $74, $75, $72, $6E, 
      $2E, $30, $39, $38, $38, $35, $32, $39, $37, $3B, $63, $61, $73, $65, $20, $38, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $39, $36, $38, $39, $35, $35, $34, $35, $37, $3B, $63, $61, $73, $65, $20, 
      $39, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $39, $31, $32, $35, $32, $37, $39, $3B, $63, $61, $73, $65, $20, $31, $30, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $38, $32, $35, $36, $38, $39, 
      $33, $36, $3B, $63, $61, $73, $65, $20, $31, $31, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $37, $31, $37, $38, $31, $39, $38, $35, $35, $3B, $63, $61, $73, $65, $20, $31, $32, $3A, $72, $65, $74, 
      $75, $72, $6E, $2E, $30, $35, $39, $39, $35, $37, $33, $35, $35, $33, $3B, $63, $61, $73, $65, $20, $31, $33, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $34, $38, $31, $31, $36, $39, $30, $33, $38, 
      $3B, $64, $65, $66, $61, $75, $6C, $74, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $33, $34, $35, $33, $37, $38, $36, $33, $3B, $7D, $7D, $68, $61, $6C, $66, $34, $20, $6D, $61, $69, $6E, $28, $66, 
      $6C, $6F, $61, $74, $32, $20, $62, $29, $7B, $66, $6C, $6F, $61, $74, $32, $20, $63, $3D, $62, $2F, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $66, $6C, $6F, $61, $74, $20, $64, $3D, $30, 
      $2E, $3B, $66, $6F, $72, $28, $69, $6E, $74, $20, $65, $3D, $30, $3B, $65, $3C, $31, $35, $3B, $65, $2B, $2B, $29, $7B, $66, $6C, $6F, $61, $74, $32, $20, $66, $3D, $63, $3B, $66, $2E, $78, $2B, $3D, 
      $28, $66, $6C, $6F, $61, $74, $28, $65, $29, $2D, $37, $2E, $35, $29, $2F, $57, $69, $64, $74, $68, $3B, $64, $2B, $3D, $66, $6C, $6F, $61, $74, $28, $49, $6E, $70, $75, $74, $2E, $65, $76, $61, $6C, 
      $28, $66, $2A, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $29, $2E, $77, $29, $2A, $61, $28, $65, $29, $3B, $7D, $72, $65, $74, $75, $72, $6E, $20, $68, $61, $6C, $66, 
      $34, $28, $66, $6C, $6F, $61, $74, $34, $28, $30, $2E, $2C, $30, $2E, $2C, $30, $2E, $2C, $31, $2E, $2D, $64, $29, $29, $3B, $7D, $0A, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Resolution', TContextShaderVariableKind.Float2, 0, 8),
      TContextShaderVariable.Create('InputResolution', TContextShaderVariableKind.Float2, 1, 8),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 2, 4)]
    ),
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FF, $FF, $FE, $FF, $29, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $7B, $00, $00, $00, $00, $02, $FF, $FF, $02, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $74, $00, $00, $00,
      $44, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $4C, $00, $00, $00, $00, $00, $00, $00, $5C, $00, $00, $00, $02, $00, $00, $00, $01, $00, $00, $00, $64, $00, $00, $00, $00, $00, $00, $00,
      $49, $6E, $70, $75, $74, $00, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $57, $69, $64, $74, $68, $00, $AB, $AB, $00, $00, $03, $00, $01, $00, $01, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65,
      $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $51, $00, $00, $05, $01, $00, $0F, $A0, $00, $00, $E0, $C0, $00, $00, $C0, $C0, $3B, $16, $45, $3D, $93, $77, $0D, $3D, $51, $00, $00, $05,
      $02, $00, $0F, $A0, $2C, $71, $C6, $3D, $6D, $73, $CA, $3D, $00, $00, $80, $3F, $00, $00, $00, $00, $51, $00, $00, $05, $03, $00, $0F, $A0, $00, $00, $A0, $C0, $D8, $95, $75, $3D, $00, $00, $80, $C0,
      $6F, $02, $93, $3D, $51, $00, $00, $05, $04, $00, $0F, $A0, $00, $00, $40, $C0, $E7, $19, $A9, $3D, $00, $00, $00, $C0, $BE, $E2, $BA, $3D, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0,
      $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $01, $00, $00, $02, $00, $00, $12, $80, $00, $00, $55, $B0, $06, $00, $00, $02, $00, $00, $04, $80, $00, $00, $00, $A0, $04, $00, $00, $04,
      $00, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $55, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $01, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $01, $00, $11, $80, $00, $00, $AA, $80,
      $01, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $02, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $02, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $00, $A0, $00, $00, $00, $B0,
      $01, $00, $00, $02, $03, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $03, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $AA, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $04, $00, $12, $80,
      $00, $00, $55, $B0, $04, $00, $00, $04, $04, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $05, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04,
      $05, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $AA, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $06, $00, $12, $80, $00, $00, $55, $B0, $02, $00, $00, $03, $06, $00, $11, $80, $00, $00, $AA, $81,
      $00, $00, $00, $B0, $01, $00, $00, $02, $07, $00, $13, $80, $00, $00, $E4, $B0, $01, $00, $00, $02, $08, $00, $12, $80, $00, $00, $55, $B0, $02, $00, $00, $03, $08, $00, $11, $80, $00, $00, $AA, $80,
      $00, $00, $00, $B0, $42, $00, $00, $03, $09, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
      $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80,
      $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
      $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $08, $00, $0F, $80, $08, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03, $00, $00, $01, $80, $09, $00, $FF, $80,
      $01, $00, $AA, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, $01, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $FF, $80, $03, $00, $55, $A0,
      $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, $03, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $04, $00, $FF, $80, $04, $00, $55, $A0,
      $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, $04, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $06, $00, $FF, $80, $02, $00, $00, $A0,
      $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $07, $00, $FF, $80, $02, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $08, $00, $FF, $80, $02, $00, $00, $A0,
      $00, $00, $00, $80, $01, $00, $00, $02, $01, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $01, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $AA, $A1, $00, $00, $00, $B0, $01, $00, $00, $02,
      $02, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $02, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $00, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $03, $00, $12, $80, $00, $00, $55, $B0,
      $04, $00, $00, $04, $03, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $AA, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $04, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $04, $00, $11, $80,
      $00, $00, $AA, $80, $03, $00, $00, $A1, $00, $00, $00, $B0, $04, $00, $00, $04, $05, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $55, $A1, $00, $00, $00, $B0, $04, $00, $00, $04, $06, $00, $11, $80,
      $00, $00, $AA, $80, $01, $00, $00, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $05, $00, $12, $80, $00, $00, $55, $B0, $01, $00, $00, $02, $06, $00, $12, $80, $00, $00, $55, $B0, $42, $00, $00, $03,
      $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80,
      $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
      $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, $04, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $02, $00, $FF, $80, $04, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, $03, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $04, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, $01, $00, $AA, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $06, $00, $FF, $80, $01, $00, $FF, $A0, $00, $00, $00, $80, $02, $00, $00, $03, $00, $00, $08, $80, $00, $00, $00, $81, $02, $00, $AA, $A0, $01, $00, $00, $02, $00, $00, $07, $80, $02, $00, $FF, $A0,
      $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX11_level_9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $22, $D1, $A6, $DE, $DA, $E4, $67, $7D, $EB, $B9, $91, $27, $69, $E6, $22, $D9, $01, $00, $00, $00, $CC, $09, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $04, $05, $00, $00,
      $D8, $07, $00, $00, $54, $08, $00, $00, $64, $09, $00, $00, $98, $09, $00, $00, $41, $6F, $6E, $39, $C4, $04, $00, $00, $C4, $04, $00, $00, $00, $02, $FF, $FF, $90, $04, $00, $00, $34, $00, $00, $00,
      $01, $00, $28, $00, $00, $00, $34, $00, $00, $00, $34, $00, $01, $00, $24, $00, $00, $00, $34, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $02, $FF, $FF,
      $51, $00, $00, $05, $01, $00, $0F, $A0, $00, $00, $E0, $C0, $00, $00, $C0, $C0, $3B, $16, $45, $3D, $93, $77, $0D, $3D, $51, $00, $00, $05, $02, $00, $0F, $A0, $2C, $71, $C6, $3D, $6D, $73, $CA, $3D,
      $00, $00, $80, $3F, $00, $00, $00, $00, $51, $00, $00, $05, $03, $00, $0F, $A0, $00, $00, $40, $C0, $E7, $19, $A9, $3D, $00, $00, $00, $C0, $BE, $E2, $BA, $3D, $51, $00, $00, $05, $04, $00, $0F, $A0,
      $00, $00, $A0, $C0, $D8, $95, $75, $3D, $00, $00, $80, $C0, $6F, $02, $93, $3D, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0,
      $01, $00, $00, $02, $00, $00, $12, $80, $00, $00, $55, $B0, $06, $00, $00, $02, $00, $00, $04, $80, $00, $00, $00, $A0, $04, $00, $00, $04, $00, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $55, $A0,
      $00, $00, $00, $B0, $01, $00, $00, $02, $01, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $01, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02,
      $02, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $02, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $03, $00, $12, $80, $00, $00, $55, $B0,
      $04, $00, $00, $04, $03, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $AA, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $04, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $04, $00, $11, $80,
      $00, $00, $AA, $80, $03, $00, $00, $A0, $00, $00, $00, $B0, $01, $00, $00, $02, $05, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $05, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $AA, $A0,
      $00, $00, $00, $B0, $01, $00, $00, $02, $06, $00, $12, $80, $00, $00, $55, $B0, $02, $00, $00, $03, $06, $00, $11, $80, $00, $00, $AA, $81, $00, $00, $00, $B0, $01, $00, $00, $02, $07, $00, $13, $80,
      $00, $00, $E4, $B0, $01, $00, $00, $02, $08, $00, $12, $80, $00, $00, $55, $B0, $02, $00, $00, $03, $08, $00, $11, $80, $00, $00, $AA, $80, $00, $00, $00, $B0, $42, $00, $00, $03, $09, $00, $0F, $80,
      $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80,
      $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $08, $00, $0F, $80, $08, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03, $00, $00, $01, $80, $09, $00, $FF, $80, $01, $00, $AA, $A0, $04, $00, $00, $04, $00, $00, $01, $80,
      $01, $00, $FF, $80, $01, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $FF, $80, $04, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $03, $00, $FF, $80, $04, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $04, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $05, $00, $FF, $80, $03, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $06, $00, $FF, $80, $02, $00, $00, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $07, $00, $FF, $80, $02, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $08, $00, $FF, $80, $02, $00, $00, $A0, $00, $00, $00, $80, $01, $00, $00, $02, $01, $00, $12, $80,
      $00, $00, $55, $B0, $04, $00, $00, $04, $01, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $AA, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $02, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04,
      $02, $00, $11, $80, $00, $00, $AA, $80, $03, $00, $00, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $03, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $03, $00, $11, $80, $00, $00, $AA, $80,
      $04, $00, $AA, $A1, $00, $00, $00, $B0, $01, $00, $00, $02, $04, $00, $12, $80, $00, $00, $55, $B0, $04, $00, $00, $04, $04, $00, $11, $80, $00, $00, $AA, $80, $04, $00, $00, $A1, $00, $00, $00, $B0,
      $04, $00, $00, $04, $05, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $55, $A1, $00, $00, $00, $B0, $04, $00, $00, $04, $06, $00, $11, $80, $00, $00, $AA, $80, $01, $00, $00, $A1, $00, $00, $00, $B0,
      $01, $00, $00, $02, $05, $00, $12, $80, $00, $00, $55, $B0, $01, $00, $00, $02, $06, $00, $12, $80, $00, $00, $55, $B0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80,
      $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0,
      $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, $03, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80,
      $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, $04, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $04, $00, $FF, $80, $04, $00, $55, $A0, $00, $00, $00, $80,
      $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, $01, $00, $AA, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $06, $00, $FF, $80, $01, $00, $FF, $A0, $00, $00, $00, $80,
      $02, $00, $00, $03, $00, $00, $08, $80, $00, $00, $00, $81, $02, $00, $AA, $A0, $01, $00, $00, $02, $00, $00, $07, $80, $02, $00, $FF, $A0, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80,
      $FF, $FF, $00, $00, $53, $48, $44, $52, $CC, $02, $00, $00, $40, $00, $00, $00, $B3, $00, $00, $00, $35, $18, $00, $00, $3E, $00, $00, $00, $93, $77, $0D, $3D, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $3B, $16, $45, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $D8, $95, $75, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $6F, $02, $93, $3D,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $E7, $19, $A9, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $BE, $E2, $BA, $3D, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $2C, $71, $C6, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $6D, $73, $CA, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $2C, $71, $C6, $3D,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $BE, $E2, $BA, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $E7, $19, $A9, $3D, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $6F, $02, $93, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $D8, $95, $75, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $3B, $16, $45, $3D,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $93, $77, $0D, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $59, $00, $00, $04, $46, $8E, $20, $00, $00, $00, $00, $00,
      $01, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $00, $00, $00, $00, $58, $18, $00, $04, $00, $70, $10, $00, $00, $00, $00, $00, $55, $55, $00, $00, $62, $10, $00, $03, $32, $10, $10, $00,
      $00, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00, $00, $00, $00, $00, $68, $00, $00, $02, $03, $00, $00, $00, $36, $20, $00, $05, $22, $00, $10, $00, $00, $00, $00, $00, $1A, $10, $10, $00,
      $00, $00, $00, $00, $36, $00, $00, $08, $C2, $00, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $30, $00, $00, $01,
      $21, $00, $00, $07, $12, $00, $10, $00, $01, $00, $00, $00, $3A, $00, $10, $00, $00, $00, $00, $00, $01, $40, $00, $00, $0F, $00, $00, $00, $03, $00, $04, $03, $0A, $00, $10, $00, $01, $00, $00, $00,
      $1E, $00, $00, $0A, $32, $00, $10, $00, $01, $00, $00, $00, $F6, $0F, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00, $F9, $FF, $FF, $FF, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $2B, $00, $00, $05, $12, $00, $10, $00, $01, $00, $00, $00, $0A, $00, $10, $00, $01, $00, $00, $00, $0E, $00, $00, $08, $12, $00, $10, $00, $01, $00, $00, $00, $0A, $00, $10, $00, $01, $00, $00, $00,
      $0A, $80, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $20, $00, $07, $12, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00, $01, $00, $00, $00, $0A, $10, $10, $00, $00, $00, $00, $00,
      $45, $00, $00, $09, $F2, $00, $10, $00, $02, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00, $00, $00, $00, $00, $00, $60, $10, $00, $00, $00, $00, $00, $32, $00, $00, $0A,
      $42, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00, $02, $00, $00, $00, $0A, $90, $90, $00, $3A, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $36, $00, $00, $05,
      $82, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00, $01, $00, $00, $00, $16, $00, $00, $01, $00, $00, $00, $08, $82, $20, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $80, $41, $00, $00, $00,
      $00, $00, $00, $00, $01, $40, $00, $00, $00, $00, $80, $3F, $36, $00, $00, $08, $72, $20, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $3E, $00, $00, $01, $53, $54, $41, $54, $74, $00, $00, $00, $10, $00, $00, $00, $03, $00, $00, $00, $0F, $00, $00, $00, $02, $00, $00, $00, $03, $00, $00, $00, $02, $00, $00, $00,
      $00, $00, $00, $00, $01, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0A, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $52, $44, $45, $46, $08, $01, $00, $00, $01, $00, $00, $00, $8C, $00, $00, $00, $03, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FF, $FF,
      $00, $11, $00, $00, $D4, $00, $00, $00, $7C, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00,
      $7C, $00, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $01, $00, $00, $00, $0C, $00, $00, $00, $82, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $49, $6E, $70, $75, $74, $00, $24, $47, $6C, $6F, $62, $61, $6C, $73, $00, $AB,
      $82, $00, $00, $00, $01, $00, $00, $00, $A4, $00, $00, $00, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $BC, $00, $00, $00, $00, $00, $00, $00, $04, $00, $00, $00, $02, $00, $00, $00,
      $C4, $00, $00, $00, $00, $00, $00, $00, $57, $69, $64, $74, $68, $00, $AB, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66,
      $74, $20, $28, $52, $29, $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E, $39, $35, $32, $2E, $32, $38, $34, $34,
      $00, $AB, $AB, $AB, $49, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00,
      $03, $03, $00, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $AB, $AB, $AB, $4F, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $53, $56, $5F, $54, $61, $72, $67, $65, $74, $00, $AB, $AB], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 4)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.Metal'}
    {$IF defined(MACOS)}
    TContextShaderSource.Create(
      TContextShaderArch.Metal,
      TEncoding.UTF8.GetBytes(
        'using namespace metal;'+

        'struct ProjectedVertex {'+
          'float4 position [[position]];'+
          'float2 textureCoord;'+
        '};'+

        'constant int g_cKernelSize = 15;'+
        'constant float weights[15] = {'+
          '0.0345378629863262,'+
          '0.0481169037520885,'+
          '0.059957355260849,'+
          '0.0717819854617119,'+
          '0.0825689360499382,'+
          '0.091252788901329,'+
          '0.0968955457210541,'+
          '0.0988529697060585,'+
          '0.0968955457210541,'+
          '0.091252788901329,'+
          '0.0825689360499382,'+
          '0.0717819854617119,'+
          '0.059957355260849,'+
          '0.0481169037520885,'+
          '0.0345378629863262};'+

        'fragment float4 fragmentShader(const ProjectedVertex in [[stage_in]],'+
                                       'constant float4 &Width [[buffer(0)]],'+
                                       'const texture2d<float> Input [[texture(0)]],'+
                                       'const sampler InputSampler [[sampler(0)]]) {'+
          'float a = 0;'+
          'float2 newCoord = 0;'+

          'for (int i = 0; i < g_cKernelSize; i++)'+
          '{'+
            'newCoord = in.textureCoord;'+
            'newCoord.x = in.textureCoord.x + ((i - (g_cKernelSize / 2) + 0.0) / Width.x);'+
            'newCoord = clamp(newCoord, 0, 1);'+
            'a += Input.sample(InputSampler, newCoord).a * weights[i];'+
          '}'+
          'return float4(0, 0, 0, 1 - a);'+
        '}'
      ),
      [TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1),
       TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.GLSL'}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $72, $65, $74, $5F, $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F,
      $54, $4D, $50, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $54, $4D, $50, $32, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $54, $4D, $50, $36, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D,
      $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $5F, $49, $6E, $70, $75, $74, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $5F, $57, $69, $64, $74, $68,
      $3B, $0D, $0A, $76, $6F, $69, $64, $20, $6D, $61, $69, $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20, $20, $66, $6C, $6F, $61, $74, $20, $5F, $61, $3B, $0D, $0A, $20, $20, $20, $20, $76, $65,
      $63, $32, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $2D, $37, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B,
      $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43,
      $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C,
      $20, $5F, $54, $4D, $50, $32, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20,
      $5F, $54, $4D, $50, $36, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $33, $2E, $34, $35, $33, $37, $38, $36, $33, $30, $45, $2D, $30, $30,
      $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $2D, $36, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32,
      $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0D, $0A, $20,
      $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0D, $0A, $20, $20,
      $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $34, $2E, $38, $31, $31, $36, $39, $30, $33, $38, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20,
      $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20,
      $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $2D, $35, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E,
      $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36,
      $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D,
      $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D,
      $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $35, $2E, $39, $39, $35, $37, $33, $35, $35, $33, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77,
      $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30,
      $2E, $78, $20, $2B, $20, $2D, $34, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28,
      $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78,
      $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74,
      $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20,
      $5F, $54, $4D, $50, $31, $2E, $77, $2A, $37, $2E, $31, $37, $38, $31, $39, $38, $35, $35, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20,
      $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $2D,
      $33, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20,
      $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28,
      $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65,
      $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E,
      $77, $2A, $38, $2E, $32, $35, $36, $38, $39, $33, $36, $30, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30,
      $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $2D, $32, $2E, $30, $2F, $5F, $57,
      $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20,
      $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20,
      $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E,
      $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $31, $32,
      $35, $32, $37, $38, $38, $39, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A,
      $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $2D, $31, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20,
      $5F, $54, $4D, $50, $32, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F,
      $54, $4D, $50, $36, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $36, $38, $39, $35, $35, $34, $35, $37,
      $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E,
      $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $30, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54,
      $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A,
      $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B,
      $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $38, $38, $35, $32, $39, $36, $39, $37, $45, $2D, $30, $30, $32, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64,
      $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $31, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D,
      $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D,
      $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F,
      $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61,
      $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $36, $38, $39, $35, $35, $34, $35, $37, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E,
      $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45,
      $58, $30, $2E, $78, $20, $2B, $20, $32, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32,
      $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61,
      $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20,
      $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B,
      $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $31, $32, $35, $32, $37, $38, $38, $39, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64,
      $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20,
      $33, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20,
      $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28,
      $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65,
      $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E,
      $77, $2A, $38, $2E, $32, $35, $36, $38, $39, $33, $36, $30, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30,
      $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $34, $2E, $30, $2F, $5F, $57, $69,
      $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F,
      $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30,
      $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70,
      $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $37, $2E, $31, $37, $38,
      $31, $39, $38, $35, $35, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20,
      $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $35, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20,
      $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72,
      $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54,
      $4D, $50, $32, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D,
      $50, $36, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $35, $2E, $39, $39, $35, $37, $33, $35, $35, $33, $45, $2D,
      $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77,
      $43, $6F, $6F, $72, $64, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $36, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50,
      $32, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20,
      $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0D, $0A,
      $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0D, $0A, $20,
      $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $34, $2E, $38, $31, $31, $36, $39, $30, $33, $38, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20,
      $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $78,
      $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $37, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $6D, $69, $6E,
      $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $36,
      $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $32, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D,
      $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $36, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D,
      $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $33, $2E, $34, $35, $33, $37, $38, $36, $33, $30, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $65, $74,
      $5F, $30, $20, $3D, $20, $76, $65, $63, $34, $28, $30, $2E, $30, $2C, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $2C, $20, $31, $2E, $30, $20, $2D, $20, $5F, $61, $29, $3B, $0D, $0A, $20, $20, $20,
      $20, $67, $6C, $5F, $46, $72, $61, $67, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $72, $65, $74, $5F, $30, $3B, $0D, $0A, $20, $20, $20, $20, $72, $65, $74, $75, $72, $6E, $3B, $0D, $0A, $7D, $20,
      $0D, $0A], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1)]
    )
    {$ENDREGION}

  ]);
  FShaders[1] := TShaderManager.RegisterShaderFromData('innerglowv.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.SKSL'}
    TContextShaderSource.Create(TContextShaderArch.SKSL, [
      $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $68, $61, $64, $65, $72, 
      $20, $49, $6E, $70, $75, $74, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, 
      $69, $66, $6F, $72, $6D, $20, $73, $68, $61, $64, $65, $72, $20, $4F, $72, $69, $67, $69, $6E, $61, $6C, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $4F, $72, $69, 
      $67, $69, $6E, $61, $6C, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $57, $69, $64, $74, $68, $3B, $75, $6E, $69, $66, 
      $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $34, $20, $46, $69, $6C, $6C, $43, $6F, $6C, $6F, $72, $3B, $69, $6E, $6C, $69, $6E, $65, $20, $66, $6C, $6F, $61, $74, $20, $61, $28, $63, $6F, $6E, $73, 
      $74, $20, $69, $6E, $74, $20, $62, $29, $7B, $73, $77, $69, $74, $63, $68, $28, $62, $29, $7B, $63, $61, $73, $65, $20, $30, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $33, $34, $35, $33, $37, $38, 
      $36, $33, $3B, $63, $61, $73, $65, $20, $31, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $34, $38, $31, $31, $36, $39, $30, $33, $38, $3B, $63, $61, $73, $65, $20, $32, $3A, $72, $65, $74, $75, $72, 
      $6E, $2E, $30, $35, $39, $39, $35, $37, $33, $35, $35, $33, $3B, $63, $61, $73, $65, $20, $33, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $37, $31, $37, $38, $31, $39, $38, $35, $35, $3B, $63, $61, 
      $73, $65, $20, $34, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $38, $32, $35, $36, $38, $39, $33, $36, $3B, $63, $61, $73, $65, $20, $35, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $39, $31, $32, 
      $35, $32, $37, $39, $3B, $63, $61, $73, $65, $20, $36, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $39, $36, $38, $39, $35, $35, $34, $35, $37, $3B, $63, $61, $73, $65, $20, $37, $3A, $72, $65, $74, 
      $75, $72, $6E, $2E, $30, $39, $38, $38, $35, $32, $39, $37, $3B, $63, $61, $73, $65, $20, $38, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $39, $36, $38, $39, $35, $35, $34, $35, $37, $3B, $63, $61, 
      $73, $65, $20, $39, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $39, $31, $32, $35, $32, $37, $39, $3B, $63, $61, $73, $65, $20, $31, $30, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $38, $32, $35, 
      $36, $38, $39, $33, $36, $3B, $63, $61, $73, $65, $20, $31, $31, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $37, $31, $37, $38, $31, $39, $38, $35, $35, $3B, $63, $61, $73, $65, $20, $31, $32, $3A, 
      $72, $65, $74, $75, $72, $6E, $2E, $30, $35, $39, $39, $35, $37, $33, $35, $35, $33, $3B, $63, $61, $73, $65, $20, $31, $33, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $34, $38, $31, $31, $36, $39, 
      $30, $33, $38, $3B, $64, $65, $66, $61, $75, $6C, $74, $3A, $72, $65, $74, $75, $72, $6E, $2E, $30, $33, $34, $35, $33, $37, $38, $36, $33, $3B, $7D, $7D, $68, $61, $6C, $66, $34, $20, $6D, $61, $69, 
      $6E, $28, $66, $6C, $6F, $61, $74, $32, $20, $62, $29, $7B, $66, $6C, $6F, $61, $74, $32, $20, $63, $3D, $62, $2F, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $66, $6C, $6F, $61, $74, $20, 
      $64, $3D, $30, $2E, $3B, $66, $6F, $72, $28, $69, $6E, $74, $20, $65, $3D, $30, $3B, $65, $3C, $31, $35, $3B, $65, $2B, $2B, $29, $7B, $66, $6C, $6F, $61, $74, $32, $20, $66, $3D, $63, $3B, $66, $2E, 
      $79, $2B, $3D, $28, $66, $6C, $6F, $61, $74, $28, $65, $29, $2D, $37, $2E, $35, $29, $2F, $57, $69, $64, $74, $68, $3B, $64, $2B, $3D, $66, $6C, $6F, $61, $74, $28, $49, $6E, $70, $75, $74, $2E, $65, 
      $76, $61, $6C, $28, $66, $2A, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $29, $2E, $77, $29, $2A, $61, $28, $65, $29, $3B, $7D, $72, $65, $74, $75, $72, $6E, $20, $68, 
      $61, $6C, $66, $34, $28, $28, $46, $69, $6C, $6C, $43, $6F, $6C, $6F, $72, $2A, $64, $29, $2A, $66, $6C, $6F, $61, $74, $28, $4F, $72, $69, $67, $69, $6E, $61, $6C, $2E, $65, $76, $61, $6C, $28, $63, 
      $2A, $4F, $72, $69, $67, $69, $6E, $61, $6C, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $29, $2E, $77, $29, $29, $3B, $7D, $0A, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Original', TContextShaderVariableKind.Texture, 1, 0),
      TContextShaderVariable.Create('Resolution', TContextShaderVariableKind.Float2, 0, 8),
      TContextShaderVariable.Create('InputResolution', TContextShaderVariableKind.Float2, 1, 8),
      TContextShaderVariable.Create('OriginalResolution', TContextShaderVariableKind.Float2, 2, 8),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 3, 4),
      TContextShaderVariable.Create('FillColor', TContextShaderVariableKind.Vector, 4, 16)]
    ),
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FF, $FF, $FE, $FF, $3C, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $C7, $00, $00, $00, $00, $02, $FF, $FF, $04, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $C0, $00, $00, $00,
      $6C, $00, $00, $00, $02, $00, $01, $00, $01, $00, $00, $00, $78, $00, $00, $00, $00, $00, $00, $00, $88, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $90, $00, $00, $00, $00, $00, $00, $00,
      $A0, $00, $00, $00, $03, $00, $01, $00, $01, $00, $00, $00, $90, $00, $00, $00, $00, $00, $00, $00, $A9, $00, $00, $00, $02, $00, $00, $00, $01, $00, $00, $00, $B0, $00, $00, $00, $00, $00, $00, $00,
      $46, $69, $6C, $6C, $43, $6F, $6C, $6F, $72, $00, $AB, $AB, $01, $00, $03, $00, $01, $00, $04, $00, $01, $00, $00, $00, $00, $00, $00, $00, $49, $6E, $70, $75, $74, $00, $AB, $AB, $04, $00, $0C, $00,
      $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $4F, $72, $69, $67, $69, $6E, $61, $6C, $00, $57, $69, $64, $74, $68, $00, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $01, $00, $00, $00,
      $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F,
      $6D, $70, $69, $6C, $65, $72, $20, $00, $51, $00, $00, $05, $02, $00, $0F, $A0, $00, $00, $E0, $C0, $00, $00, $C0, $C0, $3B, $16, $45, $3D, $93, $77, $0D, $3D, $51, $00, $00, $05, $03, $00, $0F, $A0,
      $00, $00, $A0, $C0, $D8, $95, $75, $3D, $00, $00, $80, $C0, $6F, $02, $93, $3D, $51, $00, $00, $05, $04, $00, $0F, $A0, $2C, $71, $C6, $3D, $6D, $73, $CA, $3D, $00, $00, $00, $00, $00, $00, $00, $00,
      $51, $00, $00, $05, $05, $00, $0F, $A0, $00, $00, $40, $C0, $E7, $19, $A9, $3D, $00, $00, $00, $C0, $BE, $E2, $BA, $3D, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02,
      $00, $00, $00, $90, $00, $08, $0F, $A0, $1F, $00, $00, $02, $00, $00, $00, $90, $01, $08, $0F, $A0, $01, $00, $00, $02, $00, $00, $11, $80, $00, $00, $00, $B0, $06, $00, $00, $02, $00, $00, $04, $80,
      $00, $00, $00, $A0, $04, $00, $00, $04, $00, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $55, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $01, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04,
      $01, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $02, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $02, $00, $12, $80, $00, $00, $AA, $80,
      $03, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $03, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $03, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $AA, $A0, $00, $00, $55, $B0,
      $01, $00, $00, $02, $04, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $04, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $05, $00, $11, $80,
      $00, $00, $00, $B0, $04, $00, $00, $04, $05, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $AA, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $06, $00, $11, $80, $00, $00, $00, $B0, $02, $00, $00, $03,
      $06, $00, $12, $80, $00, $00, $AA, $81, $00, $00, $55, $B0, $01, $00, $00, $02, $07, $00, $13, $80, $00, $00, $E4, $B0, $01, $00, $00, $02, $08, $00, $11, $80, $00, $00, $00, $B0, $02, $00, $00, $03,
      $08, $00, $12, $80, $00, $00, $AA, $80, $00, $00, $55, $B0, $42, $00, $00, $03, $09, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80,
      $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
      $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80,
      $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $08, $00, $0F, $80, $08, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03,
      $00, $00, $01, $80, $09, $00, $FF, $80, $02, $00, $AA, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, $02, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $02, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, $03, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $04, $00, $FF, $80, $05, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, $05, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $06, $00, $FF, $80, $04, $00, $00, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $07, $00, $FF, $80, $04, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $08, $00, $FF, $80, $04, $00, $00, $A0, $00, $00, $00, $80, $01, $00, $00, $02, $01, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $01, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $AA, $A1,
      $00, $00, $55, $B0, $01, $00, $00, $02, $02, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $02, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $00, $A1, $00, $00, $55, $B0, $01, $00, $00, $02,
      $03, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $03, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $AA, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $04, $00, $11, $80, $00, $00, $00, $B0,
      $04, $00, $00, $04, $04, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $00, $A1, $00, $00, $55, $B0, $04, $00, $00, $04, $05, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $55, $A1, $00, $00, $55, $B0,
      $04, $00, $00, $04, $06, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $00, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $05, $00, $11, $80, $00, $00, $00, $B0, $01, $00, $00, $02, $06, $00, $11, $80,
      $00, $00, $00, $B0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03,
      $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80,
      $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $00, $00, $E4, $B0, $01, $08, $E4, $A0, $04, $00, $00, $04,
      $00, $00, $01, $80, $01, $00, $FF, $80, $05, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $FF, $80, $05, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04,
      $00, $00, $01, $80, $03, $00, $FF, $80, $03, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $04, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04,
      $00, $00, $01, $80, $05, $00, $FF, $80, $02, $00, $AA, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $06, $00, $FF, $80, $02, $00, $FF, $A0, $00, $00, $00, $80, $05, $00, $00, $03,
      $00, $00, $0F, $80, $00, $00, $00, $80, $01, $00, $E4, $A0, $05, $00, $00, $03, $00, $00, $0F, $80, $07, $00, $FF, $80, $00, $00, $E4, $80, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80,
      $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('FillColor', TContextShaderVariableKind.Vector, 1, 1),
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Original', TContextShaderVariableKind.Texture, 1, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX11_level_9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $33, $E1, $52, $BC, $00, $08, $1D, $F0, $E5, $35, $8F, $35, $0E, $C1, $35, $5C, $01, $00, $00, $00, $A8, $0A, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $28, $05, $00, $00,
      $38, $08, $00, $00, $B4, $08, $00, $00, $40, $0A, $00, $00, $74, $0A, $00, $00, $41, $6F, $6E, $39, $E8, $04, $00, $00, $E8, $04, $00, $00, $00, $02, $FF, $FF, $B0, $04, $00, $00, $38, $00, $00, $00,
      $01, $00, $2C, $00, $00, $00, $38, $00, $00, $00, $38, $00, $02, $00, $24, $00, $00, $00, $38, $00, $00, $00, $00, $00, $01, $01, $01, $00, $00, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00,
      $00, $02, $FF, $FF, $51, $00, $00, $05, $02, $00, $0F, $A0, $00, $00, $E0, $C0, $00, $00, $C0, $C0, $3B, $16, $45, $3D, $93, $77, $0D, $3D, $51, $00, $00, $05, $03, $00, $0F, $A0, $00, $00, $A0, $C0,
      $D8, $95, $75, $3D, $00, $00, $80, $C0, $6F, $02, $93, $3D, $51, $00, $00, $05, $04, $00, $0F, $A0, $2C, $71, $C6, $3D, $6D, $73, $CA, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $51, $00, $00, $05,
      $05, $00, $0F, $A0, $00, $00, $40, $C0, $E7, $19, $A9, $3D, $00, $00, $00, $C0, $BE, $E2, $BA, $3D, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90,
      $00, $08, $0F, $A0, $1F, $00, $00, $02, $00, $00, $00, $90, $01, $08, $0F, $A0, $01, $00, $00, $02, $00, $00, $11, $80, $00, $00, $00, $B0, $06, $00, $00, $02, $00, $00, $04, $80, $00, $00, $00, $A0,
      $04, $00, $00, $04, $00, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $55, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $01, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $01, $00, $12, $80,
      $00, $00, $AA, $80, $02, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $02, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $02, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $00, $A0,
      $00, $00, $55, $B0, $01, $00, $00, $02, $03, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $03, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $AA, $A0, $00, $00, $55, $B0, $01, $00, $00, $02,
      $04, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $04, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $00, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $05, $00, $11, $80, $00, $00, $00, $B0,
      $04, $00, $00, $04, $05, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $AA, $A0, $00, $00, $55, $B0, $01, $00, $00, $02, $06, $00, $11, $80, $00, $00, $00, $B0, $02, $00, $00, $03, $06, $00, $12, $80,
      $00, $00, $AA, $81, $00, $00, $55, $B0, $01, $00, $00, $02, $07, $00, $13, $80, $00, $00, $E4, $B0, $01, $00, $00, $02, $08, $00, $11, $80, $00, $00, $00, $B0, $02, $00, $00, $03, $08, $00, $12, $80,
      $00, $00, $AA, $80, $00, $00, $55, $B0, $42, $00, $00, $03, $09, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80, $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80,
      $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $07, $00, $0F, $80, $07, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $08, $00, $0F, $80, $08, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03, $00, $00, $01, $80,
      $09, $00, $FF, $80, $02, $00, $AA, $A0, $04, $00, $00, $04, $00, $00, $01, $80, $01, $00, $FF, $80, $02, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $FF, $80,
      $03, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $03, $00, $FF, $80, $03, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $04, $00, $FF, $80,
      $05, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $05, $00, $FF, $80, $05, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $06, $00, $FF, $80,
      $04, $00, $00, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $07, $00, $FF, $80, $04, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $08, $00, $FF, $80,
      $04, $00, $00, $A0, $00, $00, $00, $80, $01, $00, $00, $02, $01, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $01, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $AA, $A1, $00, $00, $55, $B0,
      $01, $00, $00, $02, $02, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04, $02, $00, $12, $80, $00, $00, $AA, $80, $05, $00, $00, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $03, $00, $11, $80,
      $00, $00, $00, $B0, $04, $00, $00, $04, $03, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $AA, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $04, $00, $11, $80, $00, $00, $00, $B0, $04, $00, $00, $04,
      $04, $00, $12, $80, $00, $00, $AA, $80, $03, $00, $00, $A1, $00, $00, $55, $B0, $04, $00, $00, $04, $05, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $55, $A1, $00, $00, $55, $B0, $04, $00, $00, $04,
      $06, $00, $12, $80, $00, $00, $AA, $80, $02, $00, $00, $A1, $00, $00, $55, $B0, $01, $00, $00, $02, $05, $00, $11, $80, $00, $00, $00, $B0, $01, $00, $00, $02, $06, $00, $11, $80, $00, $00, $00, $B0,
      $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $02, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $03, $00, $0F, $80,
      $03, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $04, $00, $0F, $80, $04, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $05, $00, $0F, $80, $05, $00, $E4, $80, $00, $08, $E4, $A0,
      $42, $00, $00, $03, $06, $00, $0F, $80, $06, $00, $E4, $80, $00, $08, $E4, $A0, $42, $00, $00, $03, $07, $00, $0F, $80, $00, $00, $E4, $B0, $01, $08, $E4, $A0, $04, $00, $00, $04, $00, $00, $01, $80,
      $01, $00, $FF, $80, $05, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $02, $00, $FF, $80, $05, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $03, $00, $FF, $80, $03, $00, $FF, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $04, $00, $FF, $80, $03, $00, $55, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80,
      $05, $00, $FF, $80, $02, $00, $AA, $A0, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $01, $80, $06, $00, $FF, $80, $02, $00, $FF, $A0, $00, $00, $00, $80, $05, $00, $00, $03, $00, $00, $0F, $80,
      $00, $00, $00, $80, $01, $00, $E4, $A0, $05, $00, $00, $03, $00, $00, $0F, $80, $07, $00, $FF, $80, $00, $00, $E4, $80, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00,
      $53, $48, $44, $52, $08, $03, $00, $00, $40, $00, $00, $00, $C2, $00, $00, $00, $35, $18, $00, $00, $3E, $00, $00, $00, $93, $77, $0D, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $3B, $16, $45, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $D8, $95, $75, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $6F, $02, $93, $3D, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $E7, $19, $A9, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $BE, $E2, $BA, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $2C, $71, $C6, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $6D, $73, $CA, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $2C, $71, $C6, $3D, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $BE, $E2, $BA, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $E7, $19, $A9, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $6F, $02, $93, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $D8, $95, $75, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $3B, $16, $45, $3D, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $93, $77, $0D, $3D, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $59, $00, $00, $04, $46, $8E, $20, $00, $00, $00, $00, $00, $02, $00, $00, $00,
      $5A, $00, $00, $03, $00, $60, $10, $00, $00, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $01, $00, $00, $00, $58, $18, $00, $04, $00, $70, $10, $00, $00, $00, $00, $00, $55, $55, $00, $00,
      $58, $18, $00, $04, $00, $70, $10, $00, $01, $00, $00, $00, $55, $55, $00, $00, $62, $10, $00, $03, $32, $10, $10, $00, $00, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00, $00, $00, $00, $00,
      $68, $00, $00, $02, $03, $00, $00, $00, $36, $20, $00, $05, $12, $00, $10, $00, $00, $00, $00, $00, $0A, $10, $10, $00, $00, $00, $00, $00, $36, $00, $00, $08, $C2, $00, $10, $00, $00, $00, $00, $00,
      $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $30, $00, $00, $01, $21, $00, $00, $07, $12, $00, $10, $00, $01, $00, $00, $00, $3A, $00, $10, $00,
      $00, $00, $00, $00, $01, $40, $00, $00, $0F, $00, $00, $00, $03, $00, $04, $03, $0A, $00, $10, $00, $01, $00, $00, $00, $1E, $00, $00, $0A, $32, $00, $10, $00, $01, $00, $00, $00, $F6, $0F, $10, $00,
      $00, $00, $00, $00, $02, $40, $00, $00, $F9, $FF, $FF, $FF, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $2B, $00, $00, $05, $12, $00, $10, $00, $01, $00, $00, $00, $0A, $00, $10, $00,
      $01, $00, $00, $00, $0E, $00, $00, $08, $12, $00, $10, $00, $01, $00, $00, $00, $0A, $00, $10, $00, $01, $00, $00, $00, $0A, $80, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $20, $00, $07,
      $22, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00, $01, $00, $00, $00, $1A, $10, $10, $00, $00, $00, $00, $00, $45, $00, $00, $09, $F2, $00, $10, $00, $02, $00, $00, $00, $46, $00, $10, $00,
      $00, $00, $00, $00, $46, $7E, $10, $00, $00, $00, $00, $00, $00, $60, $10, $00, $00, $00, $00, $00, $32, $00, $00, $0A, $42, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00, $02, $00, $00, $00,
      $0A, $90, $90, $00, $3A, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $36, $00, $00, $05, $82, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00, $01, $00, $00, $00,
      $16, $00, $00, $01, $38, $00, $00, $08, $F2, $00, $10, $00, $00, $00, $00, $00, $A6, $0A, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $01, $00, $00, $00, $45, $00, $00, $09,
      $F2, $00, $10, $00, $01, $00, $00, $00, $46, $10, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00, $01, $00, $00, $00, $00, $60, $10, $00, $01, $00, $00, $00, $38, $00, $00, $07, $F2, $20, $10, $00,
      $00, $00, $00, $00, $46, $0E, $10, $00, $00, $00, $00, $00, $F6, $0F, $10, $00, $01, $00, $00, $00, $3E, $00, $00, $01, $53, $54, $41, $54, $74, $00, $00, $00, $11, $00, $00, $00, $03, $00, $00, $00,
      $0F, $00, $00, $00, $02, $00, $00, $00, $04, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $09, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $52, $44, $45, $46, $84, $01, $00, $00, $01, $00, $00, $00,
      $D4, $00, $00, $00, $05, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FF, $FF, $00, $11, $00, $00, $50, $01, $00, $00, $BC, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $C2, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $BC, $00, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $01, $00, $00, $00, $0C, $00, $00, $00,
      $C2, $00, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $01, $00, $00, $00, $01, $00, $00, $00, $0C, $00, $00, $00, $CB, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $49, $6E, $70, $75, $74, $00, $4F, $72, $69, $67, $69, $6E, $61, $6C, $00, $24,
      $47, $6C, $6F, $62, $61, $6C, $73, $00, $CB, $00, $00, $00, $02, $00, $00, $00, $EC, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $1C, $01, $00, $00, $00, $00, $00, $00,
      $04, $00, $00, $00, $02, $00, $00, $00, $24, $01, $00, $00, $00, $00, $00, $00, $34, $01, $00, $00, $10, $00, $00, $00, $10, $00, $00, $00, $02, $00, $00, $00, $40, $01, $00, $00, $00, $00, $00, $00,
      $57, $69, $64, $74, $68, $00, $AB, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $46, $69, $6C, $6C, $43, $6F, $6C, $6F, $72, $00, $AB, $AB, $01, $00, $03, $00,
      $01, $00, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F,
      $6D, $70, $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E, $39, $35, $32, $2E, $32, $38, $34, $34, $00, $AB, $AB, $AB, $49, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00,
      $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $03, $03, $00, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $AB, $AB, $AB, $4F, $53, $47, $4E,
      $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $53, $56, $5F, $54,
      $61, $72, $67, $65, $74, $00, $AB, $AB], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Original', TContextShaderVariableKind.Texture, 1, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 4),
      TContextShaderVariable.Create('FillColor', TContextShaderVariableKind.Vector, 16, 16)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.Metal'}
    {$IF defined(MACOS)}
    TContextShaderSource.Create(
      TContextShaderArch.Metal,
      TEncoding.UTF8.GetBytes(
        'using namespace metal;'+

        'struct ProjectedVertex {'+
          'float4 position [[position]];'+
          'float2 textureCoord;'+
        '};'+

        'constant int g_cKernelSize = 15;'+
        'constant float weights[15] = {'+
          '0.0345378629863262,'+
          '0.0481169037520885,'+
          '0.059957355260849,'+
          '0.0717819854617119,'+
          '0.0825689360499382,'+
          '0.091252788901329,'+
          '0.0968955457210541,'+
          '0.0988529697060585,'+
          '0.0968955457210541,'+
          '0.091252788901329,'+
          '0.0825689360499382,'+
          '0.0717819854617119,'+
          '0.059957355260849,'+
          '0.0481169037520885,'+
          '0.0345378629863262};'+

        'fragment float4 fragmentShader(const ProjectedVertex in [[stage_in]],'+
                                       'constant float4 &Width [[buffer(0)]],'+
                                       'constant float4 &FillColor [[buffer(1)]],'+
                                       'const texture2d<float> Input [[texture(0)]],'+
                                       'const sampler InputSampler [[sampler(0)]],'+
                                       'const texture2d<float> Original [[texture(1)]],'+
                                       'const sampler OriginalSampler [[sampler(1)]]) {'+
          'float a = 0;'+
          'float2 newCoord = 0;'+

          'for (int i = 0; i < g_cKernelSize; i++)'+
          '{'+
            'newCoord = in.textureCoord;'+
            'newCoord.y = in.textureCoord.y + ((i - (g_cKernelSize / 2) + 0.0) / Width.x);'+
            'newCoord = clamp(newCoord, 0, 1);'+
            'a += Input.sample(InputSampler, newCoord).a * weights[i];'+
          '}'+
          'return FillColor * a * Original.sample(OriginalSampler, in.textureCoord).a;'+
        '}'
      ),
      [TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1),
       TContextShaderVariable.Create('FillColor', TContextShaderVariableKind.Vector, 1, 1),
       TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
       TContextShaderVariable.Create('Original', TContextShaderVariableKind.Texture, 1, 0)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.GLSL'}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $72, $65, $74, $5F, $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F,
      $54, $4D, $50, $32, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $54, $4D, $50, $33, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $54,
      $4D, $50, $39, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $5F, $49, $6E, $70, $75, $74, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D,
      $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $5F, $4F, $72, $69, $67, $69, $6E, $61, $6C, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $5F, $57, $69,
      $64, $74, $68, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $34, $20, $5F, $46, $69, $6C, $6C, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $76, $6F, $69, $64, $20, $6D, $61, $69,
      $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20, $20, $66, $6C, $6F, $61, $74, $20, $5F, $61, $3B, $0D, $0A, $20, $20, $20, $20, $76, $65, $63, $32, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72,
      $64, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $37, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33,
      $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20,
      $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20,
      $20, $20, $5F, $61, $20, $3D, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $33, $2E, $34, $35, $33, $37, $38, $36, $33, $30, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65,
      $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58,
      $30, $2E, $79, $20, $2B, $20, $2D, $36, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32,
      $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61,
      $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20,
      $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B,
      $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $34, $2E, $38, $31, $31, $36, $39, $30, $33, $38, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64,
      $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20,
      $2D, $35, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C,
      $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32,
      $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72,
      $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31,
      $2E, $77, $2A, $35, $2E, $39, $39, $35, $37, $33, $35, $35, $33, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58,
      $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $34, $2E, $30, $2F, $5F,
      $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C,
      $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C,
      $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49,
      $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $37, $2E, $31,
      $37, $38, $31, $39, $38, $35, $35, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $33, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B,
      $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43,
      $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C,
      $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20,
      $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $38, $2E, $32, $35, $36, $38, $39, $33, $36,
      $30, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F,
      $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $32, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20,
      $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B,
      $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33,
      $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29,
      $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $31, $32, $35, $32, $37, $38, $38, $39, $45, $2D, $30, $30, $32,
      $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F,
      $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $31, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20,
      $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20,
      $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20,
      $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $36, $38, $39, $35, $35, $34, $35, $37, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D,
      $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $30, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76,
      $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D,
      $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31,
      $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F,
      $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $38, $38, $35, $32, $39, $36, $39, $37, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79,
      $20, $2B, $20, $31, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E,
      $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65,
      $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74,
      $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D,
      $50, $31, $2E, $77, $2A, $39, $2E, $36, $38, $39, $35, $35, $34, $35, $37, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54,
      $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $32, $2E, $30, $2F,
      $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29,
      $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30,
      $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F,
      $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E,
      $31, $32, $35, $32, $37, $38, $38, $39, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B,
      $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $33, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B,
      $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43,
      $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C,
      $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20,
      $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $38, $2E, $32, $35, $36, $38, $39, $33, $36,
      $30, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F,
      $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $34, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F,
      $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29,
      $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B,
      $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $37, $2E, $31, $37, $38, $31, $39, $38, $35, $35, $45, $2D, $30, $30, $32, $3B,
      $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72,
      $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $35, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20,
      $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54,
      $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20,
      $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F,
      $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $35, $2E, $39, $39, $35, $37, $33, $35, $35, $33, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F,
      $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54,
      $45, $58, $30, $2E, $79, $20, $2B, $20, $36, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63,
      $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D,
      $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D,
      $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20,
      $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $34, $2E, $38, $31, $31, $36, $39, $30, $33, $38, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72,
      $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B,
      $20, $37, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C,
      $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32,
      $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72,
      $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31,
      $2E, $77, $2A, $33, $2E, $34, $35, $33, $37, $38, $36, $33, $30, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65,
      $32, $44, $28, $5F, $4F, $72, $69, $67, $69, $6E, $61, $6C, $2C, $20, $54, $45, $58, $30, $2E, $78, $79, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $65, $74, $5F, $30, $20, $3D, $20, $5F, $46,
      $69, $6C, $6C, $43, $6F, $6C, $6F, $72, $2A, $5F, $61, $2A, $5F, $54, $4D, $50, $32, $2E, $77, $3B, $0D, $0A, $20, $20, $20, $20, $67, $6C, $5F, $46, $72, $61, $67, $43, $6F, $6C, $6F, $72, $20, $3D,
      $20, $5F, $72, $65, $74, $5F, $30, $3B, $0D, $0A, $20, $20, $20, $20, $72, $65, $74, $75, $72, $6E, $3B, $0D, $0A, $7D, $20, $0D, $0A], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Original', TContextShaderVariableKind.Texture, 1, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1),
      TContextShaderVariable.Create('FillColor', TContextShaderVariableKind.Vector, 0, 1)]
    ),
    TContextShaderSource.Create(TContextShaderArch.Mac, [
      $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $72, $65, $74, $5F, $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F,
      $54, $4D, $50, $32, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $54, $4D, $50, $33, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $54,
      $4D, $50, $39, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $63, $30, $30, $31, $38, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $5F, $49,
      $6E, $70, $75, $74, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $5F, $4F, $72, $69, $67, $69, $6E, $61, $6C, $3B, $0D, $0A, $75, $6E, $69,
      $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $34, $20, $5F, $46, $69, $6C, $6C, $43, $6F,
      $6C, $6F, $72, $3B, $0D, $0A, $76, $6F, $69, $64, $20, $6D, $61, $69, $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20, $20, $66, $6C, $6F, $61, $74, $20, $5F, $61, $3B, $0D, $0A, $20, $20, $20,
      $20, $76, $65, $63, $32, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78,
      $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $37, $2E, $30, $2F, $5F, $57, $69, $64,
      $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E,
      $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E,
      $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75,
      $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $33, $2E, $34, $35, $33, $37, $38, $36, $33, $30, $45,
      $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65,
      $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $36, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54,
      $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A,
      $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B,
      $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $34, $2E, $38, $31, $31, $36, $39, $30, $33, $38, $45, $2D, $30, $30, $32, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64,
      $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $35, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20,
      $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54,
      $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20,
      $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F,
      $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $35, $2E, $39, $39, $35, $37, $33, $35, $35, $33, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F,
      $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54,
      $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $34, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65,
      $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20,
      $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20,
      $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61,
      $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $37, $2E, $31, $37, $38, $31, $39, $38, $35, $35, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F,
      $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20,
      $2B, $20, $2D, $33, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E,
      $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65,
      $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74,
      $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D,
      $50, $31, $2E, $77, $2A, $38, $2E, $32, $35, $36, $38, $39, $33, $36, $30, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54,
      $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $32, $2E, $30,
      $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30,
      $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E,
      $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28,
      $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39,
      $2E, $31, $32, $35, $32, $37, $38, $38, $39, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79,
      $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $2D, $31, $2E, $30, $2F, $5F, $57, $69, $64, $74,
      $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65,
      $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30,
      $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74,
      $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $36, $38, $39, $35, $35,
      $34, $35, $37, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $30, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29,
      $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50,
      $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39,
      $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $38, $38, $35, $32, $39, $36, $39, $37, $45, $2D, $30, $30,
      $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $31, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20,
      $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20,
      $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20,
      $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $36, $38, $39, $35, $35, $34, $35, $37, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D,
      $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $32, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76,
      $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D,
      $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31,
      $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F,
      $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $39, $2E, $31, $32, $35, $32, $37, $38, $38, $39, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F,
      $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79,
      $20, $2B, $20, $33, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E,
      $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65,
      $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74,
      $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D,
      $50, $31, $2E, $77, $2A, $38, $2E, $32, $35, $36, $38, $39, $33, $36, $30, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54,
      $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $34, $2E, $30, $2F,
      $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29,
      $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30,
      $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F,
      $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $37, $2E,
      $31, $37, $38, $31, $39, $38, $35, $35, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B,
      $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $35, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B,
      $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43,
      $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C,
      $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20,
      $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $35, $2E, $39, $39, $35, $37, $33, $35, $35,
      $33, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F,
      $6E, $65, $77, $43, $6F, $6F, $72, $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $36, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F,
      $54, $4D, $50, $33, $20, $3D, $20, $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29,
      $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B,
      $0D, $0A, $20, $20, $20, $20, $5F, $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $34, $2E, $38, $31, $31, $36, $39, $30, $33, $38, $45, $2D, $30, $30, $32, $3B,
      $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72,
      $64, $2E, $79, $20, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $2B, $20, $37, $2E, $30, $2F, $5F, $57, $69, $64, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20, $3D, $20,
      $6D, $69, $6E, $28, $76, $65, $63, $32, $28, $20, $31, $2E, $30, $2C, $20, $31, $2E, $30, $29, $2C, $20, $5F, $6E, $65, $77, $43, $6F, $6F, $72, $64, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54,
      $4D, $50, $39, $20, $3D, $20, $6D, $61, $78, $28, $76, $65, $63, $32, $28, $20, $30, $2E, $30, $2C, $20, $30, $2E, $30, $29, $2C, $20, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20,
      $5F, $54, $4D, $50, $31, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $54, $4D, $50, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F,
      $61, $20, $3D, $20, $5F, $61, $20, $2B, $20, $5F, $54, $4D, $50, $31, $2E, $77, $2A, $33, $2E, $34, $35, $33, $37, $38, $36, $33, $30, $45, $2D, $30, $30, $32, $3B, $0D, $0A, $20, $20, $20, $20, $5F,
      $63, $30, $30, $31, $38, $20, $3D, $20, $76, $65, $63, $32, $28, $54, $45, $58, $30, $2E, $78, $2C, $20, $31, $2E, $30, $20, $2D, $20, $54, $45, $58, $30, $2E, $79, $29, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $4F, $72, $69, $67, $69, $6E, $61, $6C, $2C, $20, $5F, $63, $30, $30, $31, $38, $29, $3B, $0D, $0A,
      $20, $20, $20, $20, $5F, $72, $65, $74, $5F, $30, $20, $3D, $20, $5F, $46, $69, $6C, $6C, $43, $6F, $6C, $6F, $72, $2A, $5F, $61, $2A, $5F, $54, $4D, $50, $32, $2E, $77, $3B, $0D, $0A, $20, $20, $20,
      $20, $67, $6C, $5F, $46, $72, $61, $67, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $72, $65, $74, $5F, $30, $3B, $0D, $0A, $20, $20, $20, $20, $72, $65, $74, $75, $72, $6E, $3B, $0D, $0A, $7D, $20,
      $0D, $0A], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Original', TContextShaderVariableKind.Texture, 1, 0),
      TContextShaderVariable.Create('Width', TContextShaderVariableKind.Float, 0, 1),
      TContextShaderVariable.Create('FillColor', TContextShaderVariableKind.Vector, 0, 1)]
    )
    {$ENDREGION}

  ]);
  FPassCount := 2;
end;

destructor TInnerGlowFilter.Destroy;
begin
  inherited;
end;

procedure TInnerGlowFilter.LoadShaders;
const
  Scale = 1.5;
var
  C: TAlphaColorRec;
begin
  FilterContext.SetShaders(FVertexShader, FShaders[FPass]);
  if FPass = 0 then
    FilterContext.SetShaderVariable('Width', [Vector3D(InputSize.Width / ValuesAsFloat['BlurAmount'] / Scale, 0, 0, 0)]);
  if FPass = 1 then
  begin
    FilterContext.SetShaderVariable('Width', [Vector3D(InputSize.Height / ValuesAsFloat['BlurAmount'] / Scale, 0, 0, 0)]);
    FilterContext.SetInputToShaderVariable('Original');
    C := TAlphaColorRec(ValuesAsColor['Color']);
    FilterContext.SetShaderVariable('FillColor', [Vector3D(C.R / $FF, C.G / $FF, C.B / $FF, C.A / $FF)]);
  end;
end;

class function TInnerGlowFilter.FilterAttr: TFilterRec;
begin
  Result := TFilterRec.Create('InnerGlowFilter', 'An effect that add glow.', [
    TFilterValueRec.Create('BlurAmount', 'The blur factor.', TFilterValueType.Float, 0.7, 0.01, 10),
    TFilterValueRec.Create('Color', 'The glow color.', TFilterValueType.Color, $FFFFD700, 0, 0)
  ]);
end;

{ TSwipeFilter }

procedure TSwipeFilter.CalcSize(var W, H: Integer);
var
  Dx, Dy: Single;
  MPoint: TPointF;
  M1Point: TPointF;
  V: TPointF;
  A, B, B1: Single;
  Kx, Ky: Single;

  procedure CorrectWhenMouseLeaveImage;
  var
    LCorrector : TPointF ;
    La1,Lb1,La2,Lb2 : Double ;
    LTempPoint : TPointF ;
    LQ : TPointF ;
  begin
    LCorrector := PointF(0,0);
    if (MPoint.X < FA1Point.X) and (MPoint.X < 0) then
      LCorrector.X := +W;
    if (MPoint.Y < FA1Point.Y) and (MPoint.Y < 0) then
      LCorrector.Y := +H;
    if (MPoint.X > FA1Point.X) and (MPoint.X > W) then
      LCorrector.X := -W;
    if (MPoint.Y > FA1Point.Y) and (MPoint.Y > H) then
      LCorrector.Y := -H;
    LCorrector := LCorrector + FA1Point ;
    LTempPoint := MPoint - FA1Point;
    La1 := LTempPoint.Y / LTempPoint.X ;
    Lb1 := LCorrector.Y - LCorrector.X * La1;
    La2 := - LTempPoint.X / LTempPoint.Y ;
    Lb2 := MPoint.Y - MPoint.X * La2;
    LQ.X := (Lb2 - Lb1)/(La1 - La2) ;
    LQ.Y := La1 * LQ.X + Lb1;
    FAPoint := LCorrector - (LCorrector - LQ) * 2;
    FA1Point := LCorrector;
    MPoint := FAPoint + FA1Point;
    MPoint := PointF(MPoint.X/2, MPoint.Y/2);
  end;

begin
  inherited;
  if (H = 0) or (W = 0) then
    Exit;
  FK := W / H ;
  FAPoint := ValuesAsPoint['MousePoint'] ;
  FA1Point := ValuesAsPoint['CornerPoint'] ;
  {$IFNDEF MSWINDOWS}
  if not GlobalUseMetal then begin
    FAPoint.Y := H - FAPoint.Y;
    FA1Point.Y := H - FA1Point.Y;
  end;
  {$ENDIF}
  if FAPoint.X = W then
    FAPoint.X := W - 0.1;
  if FAPoint.X = 0 then
    FAPoint.X := 0.1;
  if FAPoint.Y = H then
    FAPoint.Y := H - 0.1;
  if FAPoint.Y = 0 then
    FAPoint.Y := 0.1;
  Dx := FA1Point.X - W / 2;
  Dy := FA1Point.Y - H / 2;
  if Dx > 0 then
    Dx := W
  else
    Dx := 0;
  if Dy > 0 then
    Dy := H
  else
    Dy := 0;
  FA1Point := TPointF.Create(Dx, Dy);
  FLength := ValuesAsFloat['Deep'];
  MPoint := FAPoint + FA1Point;
  MPoint := TPointF.Create(MPoint.X / 2, MPoint.Y / 2);
  if (MPoint.X < 0) or (MPoint.Y < 0) or (MPoint.X > W) or (MPoint.Y > H) then
    CorrectWhenMouseLeaveImage;
  V := FAPoint - FA1Point;
  V := TPointF.Create(V.X / 2, V.Y / 2);
  if V.Y = 0 then
    Exit ;
  A := - V.X / V.Y;
  B := MPoint.Y - MPoint.X * A;
  FBPoint := TPointF.Create(FA1Point.X, A * FA1Point.X + B);
  if A = 0 then
    Exit ;
  FCPoint := TPointF.Create((FA1Point.y - B) / A, FA1Point.Y);
  FK := FLength / V.Length;
  Kx := V.X / V.Length;
  Ky := V.Y / V.Length;
  M1Point := MPoint + TPointF.Create(FLength * Kx, FLength * Ky) ;
  B1 := M1Point.Y - M1Point.X * A;
  A := A * W / H;
  FAPoint := TPointF.Create(FAPoint.X / W, FAPoint.Y / H);
  FA1Point := TPointF.Create(FA1Point.X / W, FA1Point.Y / H);
  FBPoint := TPointF.Create(FBPoint.X / W, FBPoint.Y / H);
  FCPoint := TPointF.Create(FCPoint.X / W, FCPoint.Y / H);
  FLineAB := TPointF.Create(A, B / H);
  FLineAB1 := TPointF.Create(A, B1 / H);
  FK := W / H;
  FLength := FLength / W;
end;

constructor TSwipeFilter.Create;
begin
  inherited;
  FNeedInternalSecondTex := '';
  FShaders[0] := TShaderManager.RegisterShaderFromData('SwipeFlatTransition.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.SKSL'}
    TContextShaderSource.Create(TContextShaderArch.SKSL, [
      $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $68, $61, $64, $65, $72, 
      $20, $49, $6E, $70, $75, $74, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, 
      $69, $66, $6F, $72, $6D, $20, $73, $68, $61, $64, $65, $72, $20, $54, $61, $72, $67, $65, $74, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $54, $61, $72, $67, $65, 
      $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $68, $61, $64, $65, $72, $20, $42, $61, $63, $6B, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, 
      $66, $6C, $6F, $61, $74, $32, $20, $42, $61, $63, $6B, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $41, $50, $6F, 
      $69, $6E, $74, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $41, $31, $50, $6F, $69, $6E, $74, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, 
      $32, $20, $42, $50, $6F, $69, $6E, $74, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $43, $50, $6F, $69, $6E, $74, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, 
      $6C, $6F, $61, $74, $32, $20, $4C, $69, $6E, $65, $41, $42, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $4C, $69, $6E, $65, $41, $42, $31, $3B, $75, $6E, $69, $66, 
      $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $4B, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $4C, $65, $6E, $3B, $66, $6C, $6F, $61, $74, $20, $61, $28, $63, $6F, 
      $6E, $73, $74, $20, $66, $6C, $6F, $61, $74, $32, $20, $62, $2C, $63, $6F, $6E, $73, $74, $20, $66, $6C, $6F, $61, $74, $32, $20, $63, $29, $7B, $72, $65, $74, $75, $72, $6E, $20, $62, $2E, $78, $2A, 
      $63, $2E, $79, $2D, $63, $2E, $78, $2A, $62, $2E, $79, $3B, $7D, $62, $6F, $6F, $6C, $20, $62, $28, $63, $6F, $6E, $73, $74, $20, $66, $6C, $6F, $61, $74, $32, $20, $63, $2C, $63, $6F, $6E, $73, $74, 
      $20, $66, $6C, $6F, $61, $74, $32, $20, $64, $29, $7B, $72, $65, $74, $75, $72, $6E, $20, $61, $28, $64, $2D, $63, $2C, $42, $50, $6F, $69, $6E, $74, $2D, $63, $29, $2A, $61, $28, $42, $50, $6F, $69, 
      $6E, $74, $2D, $63, $2C, $43, $50, $6F, $69, $6E, $74, $2D, $63, $29, $3E, $30, $2E, $26, $26, $61, $28, $42, $50, $6F, $69, $6E, $74, $2D, $63, $2C, $43, $50, $6F, $69, $6E, $74, $2D, $63, $29, $2A, 
      $61, $28, $43, $50, $6F, $69, $6E, $74, $2D, $63, $2C, $64, $2D, $63, $29, $3E, $30, $2E, $3B, $7D, $66, $6C, $6F, $61, $74, $34, $20, $63, $28, $63, $6F, $6E, $73, $74, $20, $66, $6C, $6F, $61, $74, 
      $32, $20, $64, $29, $7B, $72, $65, $74, $75, $72, $6E, $20, $66, $6C, $6F, $61, $74, $34, $28, $49, $6E, $70, $75, $74, $2E, $65, $76, $61, $6C, $28, $64, $2A, $49, $6E, $70, $75, $74, $52, $65, $73, 
      $6F, $6C, $75, $74, $69, $6F, $6E, $29, $29, $3B, $7D, $66, $6C, $6F, $61, $74, $34, $20, $64, $28, $63, $6F, $6E, $73, $74, $20, $66, $6C, $6F, $61, $74, $32, $20, $65, $29, $7B, $72, $65, $74, $75, 
      $72, $6E, $20, $66, $6C, $6F, $61, $74, $34, $28, $54, $61, $72, $67, $65, $74, $2E, $65, $76, $61, $6C, $28, $65, $2A, $54, $61, $72, $67, $65, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, 
      $29, $29, $3B, $7D, $66, $6C, $6F, $61, $74, $32, $20, $65, $28, $66, $6C, $6F, $61, $74, $32, $20, $66, $29, $7B, $66, $2E, $78, $2A, $3D, $4B, $3B, $72, $65, $74, $75, $72, $6E, $20, $66, $3B, $7D, 
      $66, $6C, $6F, $61, $74, $32, $20, $66, $28, $66, $6C, $6F, $61, $74, $32, $20, $67, $29, $7B, $67, $2E, $78, $2F, $3D, $4B, $3B, $72, $65, $74, $75, $72, $6E, $20, $67, $3B, $7D, $66, $6C, $6F, $61, 
      $74, $34, $20, $67, $28, $66, $6C, $6F, $61, $74, $32, $20, $68, $29, $7B, $66, $6C, $6F, $61, $74, $32, $20, $69, $3D, $65, $28, $41, $50, $6F, $69, $6E, $74, $29, $3B, $66, $6C, $6F, $61, $74, $32, 
      $20, $6A, $3D, $65, $28, $41, $31, $50, $6F, $69, $6E, $74, $29, $3B, $68, $3D, $65, $28, $68, $29, $3B, $66, $6C, $6F, $61, $74, $32, $20, $6B, $3D, $6E, $6F, $72, $6D, $61, $6C, $69, $7A, $65, $28, 
      $69, $2D, $6A, $29, $3B, $68, $3D, $72, $65, $66, $6C, $65, $63, $74, $28, $68, $2C, $6B, $29, $3B, $66, $6C, $6F, $61, $74, $32, $20, $6C, $3D, $72, $65, $66, $6C, $65, $63, $74, $28, $6A, $2C, $6B, 
      $29, $3B, $66, $6C, $6F, $61, $74, $32, $20, $6D, $3D, $6C, $3B, $68, $2D, $3D, $6D, $2D, $69, $3B, $72, $65, $74, $75, $72, $6E, $20, $66, $6C, $6F, $61, $74, $34, $28, $42, $61, $63, $6B, $2E, $65, 
      $76, $61, $6C, $28, $66, $28, $68, $29, $2A, $42, $61, $63, $6B, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $29, $29, $3B, $7D, $62, $6F, $6F, $6C, $20, $68, $28, $63, $6F, $6E, $73, $74, $20, 
      $66, $6C, $6F, $61, $74, $32, $20, $69, $29, $7B, $72, $65, $74, $75, $72, $6E, $20, $4C, $69, $6E, $65, $41, $42, $2E, $78, $2A, $69, $2E, $78, $2B, $4C, $69, $6E, $65, $41, $42, $2E, $79, $3C, $3D, 
      $69, $2E, $79, $26, $26, $4C, $69, $6E, $65, $41, $42, $31, $2E, $78, $2A, $69, $2E, $78, $2B, $4C, $69, $6E, $65, $41, $42, $31, $2E, $79, $3E, $3D, $69, $2E, $79, $7C, $7C, $4C, $69, $6E, $65, $41, 
      $42, $2E, $78, $2A, $69, $2E, $78, $2B, $4C, $69, $6E, $65, $41, $42, $2E, $79, $3E, $3D, $69, $2E, $79, $26, $26, $4C, $69, $6E, $65, $41, $42, $31, $2E, $78, $2A, $69, $2E, $78, $2B, $4C, $69, $6E, 
      $65, $41, $42, $31, $2E, $79, $3C, $3D, $69, $2E, $79, $3B, $7D, $66, $6C, $6F, $61, $74, $32, $20, $69, $28, $63, $6F, $6E, $73, $74, $20, $66, $6C, $6F, $61, $74, $32, $20, $6A, $29, $7B, $66, $6C, 
      $6F, $61, $74, $32, $20, $6B, $3D, $66, $6C, $6F, $61, $74, $32, $28, $28, $6A, $2E, $79, $2D, $4C, $69, $6E, $65, $41, $42, $31, $2E, $79, $29, $2F, $4C, $69, $6E, $65, $41, $42, $31, $2E, $78, $2C, 
      $6A, $2E, $79, $29, $3B, $66, $6C, $6F, $61, $74, $20, $6C, $3D, $6C, $65, $6E, $67, $74, $68, $28, $41, $50, $6F, $69, $6E, $74, $2D, $41, $31, $50, $6F, $69, $6E, $74, $29, $2F, $28, $43, $50, $6F, 
      $69, $6E, $74, $2D, $41, $31, $50, $6F, $69, $6E, $74, $29, $2E, $78, $3B, $66, $6C, $6F, $61, $74, $20, $6D, $3D, $6C, $2A, $28, $6B, $2D, $6A, $29, $2E, $78, $3B, $66, $6C, $6F, $61, $74, $20, $6E, 
      $3D, $6D, $2F, $4C, $65, $6E, $3B, $66, $6C, $6F, $61, $74, $20, $6F, $3D, $28, $28, $28, $6E, $2A, $6E, $29, $2A, $6E, $29, $2A, $6D, $29, $2A, $2E, $31, $3B, $66, $6C, $6F, $61, $74, $32, $20, $70, 
      $3D, $6F, $2A, $6E, $6F, $72, $6D, $61, $6C, $69, $7A, $65, $28, $41, $31, $50, $6F, $69, $6E, $74, $2D, $41, $50, $6F, $69, $6E, $74, $29, $3B, $72, $65, $74, $75, $72, $6E, $20, $6A, $2B, $70, $3B, 
      $7D, $66, $6C, $6F, $61, $74, $34, $20, $6A, $28, $63, $6F, $6E, $73, $74, $20, $66, $6C, $6F, $61, $74, $32, $20, $6B, $2C, $63, $6F, $6E, $73, $74, $20, $66, $6C, $6F, $61, $74, $32, $20, $6C, $29, 
      $7B, $69, $66, $28, $62, $28, $6B, $2C, $41, $50, $6F, $69, $6E, $74, $29, $29, $7B, $72, $65, $74, $75, $72, $6E, $20, $67, $28, $6B, $29, $3B, $7D, $69, $66, $28, $62, $28, $6B, $2C, $41, $31, $50, 
      $6F, $69, $6E, $74, $29, $29, $7B, $72, $65, $74, $75, $72, $6E, $20, $64, $28, $6C, $29, $3B, $7D, $72, $65, $74, $75, $72, $6E, $20, $63, $28, $6B, $29, $3B, $7D, $68, $61, $6C, $66, $34, $20, $6D, 
      $61, $69, $6E, $28, $66, $6C, $6F, $61, $74, $32, $20, $6B, $29, $7B, $66, $6C, $6F, $61, $74, $32, $20, $6C, $3D, $6B, $2F, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $66, $6C, $6F, $61, 
      $74, $34, $20, $6D, $3D, $66, $6C, $6F, $61, $74, $34, $28, $49, $6E, $70, $75, $74, $2E, $65, $76, $61, $6C, $28, $6C, $2A, $49, $6E, $70, $75, $74, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, 
      $29, $29, $3B, $69, $66, $28, $68, $28, $6C, $29, $29, $7B, $66, $6C, $6F, $61, $74, $32, $20, $6E, $3D, $69, $28, $6C, $29, $3B, $69, $66, $28, $28, $28, $6E, $2E, $78, $3C, $30, $2E, $7C, $7C, $6E, 
      $2E, $79, $3C, $30, $2E, $29, $7C, $7C, $6E, $2E, $78, $3E, $31, $2E, $29, $7C, $7C, $6E, $2E, $79, $3E, $31, $2E, $29, $7B, $6D, $3D, $64, $28, $6C, $29, $3B, $7D, $65, $6C, $73, $65, $7B, $6D, $3D, 
      $6A, $28, $6E, $2C, $6C, $29, $3B, $7D, $69, $66, $28, $61, $62, $73, $28, $28, $6C, $2E, $78, $2A, $4C, $69, $6E, $65, $41, $42, $31, $2E, $78, $2B, $4C, $69, $6E, $65, $41, $42, $31, $2E, $79, $29, 
      $2D, $6C, $2E, $79, $29, $3C, $2E, $30, $30, $31, $29, $7B, $6D, $3D, $6A, $28, $6C, $2C, $6C, $29, $3B, $7D, $7D, $65, $6C, $73, $65, $7B, $6D, $3D, $6A, $28, $6C, $2C, $6C, $29, $3B, $7D, $72, $65, 
      $74, $75, $72, $6E, $20, $68, $61, $6C, $66, $34, $28, $6D, $29, $3B, $7D, $0A, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Target', TContextShaderVariableKind.Texture, 1, 0),
      TContextShaderVariable.Create('Back', TContextShaderVariableKind.Texture, 2, 0),
      TContextShaderVariable.Create('Resolution', TContextShaderVariableKind.Float2, 0, 8),
      TContextShaderVariable.Create('InputResolution', TContextShaderVariableKind.Float2, 1, 8),
      TContextShaderVariable.Create('TargetResolution', TContextShaderVariableKind.Float2, 2, 8),
      TContextShaderVariable.Create('BackResolution', TContextShaderVariableKind.Float2, 3, 8),
      TContextShaderVariable.Create('APoint', TContextShaderVariableKind.Float2, 4, 8),
      TContextShaderVariable.Create('A1Point', TContextShaderVariableKind.Float2, 5, 8),
      TContextShaderVariable.Create('BPoint', TContextShaderVariableKind.Float2, 6, 8),
      TContextShaderVariable.Create('CPoint', TContextShaderVariableKind.Float2, 7, 8),
      TContextShaderVariable.Create('LineAB', TContextShaderVariableKind.Float2, 8, 8),
      TContextShaderVariable.Create('LineAB1', TContextShaderVariableKind.Float2, 9, 8),
      TContextShaderVariable.Create('K', TContextShaderVariableKind.Float, 10, 4),
      TContextShaderVariable.Create('Len', TContextShaderVariableKind.Float, 11, 4)]
    ),
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FF, $FF, $FE, $FF, $54, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $26, $01, $00, $00, $00, $02, $FF, $FF, $08, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $1F, $01, $00, $00,
      $BC, $00, $00, $00, $02, $00, $01, $00, $01, $00, $00, $00, $C4, $00, $00, $00, $00, $00, $00, $00, $D4, $00, $00, $00, $02, $00, $00, $00, $01, $00, $00, $00, $C4, $00, $00, $00, $00, $00, $00, $00,
      $DB, $00, $00, $00, $02, $00, $02, $00, $01, $00, $00, $00, $C4, $00, $00, $00, $00, $00, $00, $00, $E2, $00, $00, $00, $03, $00, $02, $00, $01, $00, $0A, $00, $E8, $00, $00, $00, $00, $00, $00, $00,
      $F8, $00, $00, $00, $02, $00, $03, $00, $01, $00, $00, $00, $C4, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $03, $00, $00, $00, $01, $00, $02, $00, $E8, $00, $00, $00, $00, $00, $00, $00,
      $05, $01, $00, $00, $02, $00, $04, $00, $01, $00, $00, $00, $08, $01, $00, $00, $00, $00, $00, $00, $18, $01, $00, $00, $03, $00, $01, $00, $01, $00, $06, $00, $E8, $00, $00, $00, $00, $00, $00, $00,
      $41, $31, $50, $6F, $69, $6E, $74, $00, $01, $00, $03, $00, $01, $00, $02, $00, $01, $00, $00, $00, $00, $00, $00, $00, $41, $50, $6F, $69, $6E, $74, $00, $42, $50, $6F, $69, $6E, $74, $00, $42, $61,
      $63, $6B, $00, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $43, $50, $6F, $69, $6E, $74, $00, $49, $6E, $70, $75, $74, $00, $4B, $00, $AB, $00, $00, $03, $00,
      $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $54, $61, $72, $67, $65, $74, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20,
      $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $AB, $51, $00, $00, $05, $05, $00, $0F, $A0, $00, $00, $00, $00, $00, $00, $80, $3F,
      $00, $00, $00, $00, $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $1F, $00, $00, $02, $00, $00, $00, $90,
      $01, $08, $0F, $A0, $1F, $00, $00, $02, $00, $00, $00, $90, $02, $08, $0F, $A0, $01, $00, $00, $02, $00, $00, $08, $80, $00, $00, $00, $A0, $05, $00, $00, $03, $00, $00, $02, $80, $00, $00, $FF, $80,
      $04, $00, $00, $A0, $01, $00, $00, $02, $00, $00, $04, $80, $00, $00, $55, $A0, $01, $00, $00, $02, $00, $00, $01, $80, $01, $00, $00, $A0, $05, $00, $00, $03, $01, $00, $02, $80, $00, $00, $00, $80,
      $04, $00, $00, $A0, $01, $00, $00, $02, $01, $00, $04, $80, $01, $00, $55, $A0, $02, $00, $00, $03, $02, $00, $06, $80, $00, $00, $E4, $80, $01, $00, $E4, $81, $5A, $00, $00, $04, $00, $00, $01, $80,
      $02, $00, $C9, $80, $02, $00, $C9, $80, $05, $00, $00, $A0, $07, $00, $00, $02, $00, $00, $01, $80, $00, $00, $00, $80, $05, $00, $00, $03, $02, $00, $06, $80, $02, $00, $E4, $80, $00, $00, $00, $80,
      $5A, $00, $00, $04, $00, $00, $01, $80, $01, $00, $C9, $80, $02, $00, $C9, $80, $05, $00, $00, $A0, $02, $00, $00, $03, $00, $00, $01, $80, $00, $00, $00, $80, $00, $00, $00, $80, $04, $00, $00, $04,
      $01, $00, $06, $80, $02, $00, $E4, $80, $00, $00, $00, $81, $01, $00, $E4, $80, $02, $00, $00, $03, $00, $00, $06, $80, $00, $00, $E4, $81, $01, $00, $E4, $80, $05, $00, $00, $03, $01, $00, $02, $80,
      $00, $00, $00, $B0, $04, $00, $00, $A0, $01, $00, $00, $02, $01, $00, $04, $80, $00, $00, $55, $B0, $5A, $00, $00, $04, $00, $00, $01, $80, $01, $00, $C9, $80, $02, $00, $C9, $80, $05, $00, $00, $A0,
      $02, $00, $00, $03, $00, $00, $01, $80, $00, $00, $00, $80, $00, $00, $00, $80, $04, $00, $00, $04, $01, $00, $06, $80, $02, $00, $E4, $80, $00, $00, $00, $81, $01, $00, $E4, $80, $02, $00, $00, $03,
      $00, $00, $06, $80, $00, $00, $E4, $81, $01, $00, $E4, $80, $06, $00, $00, $02, $00, $00, $01, $80, $04, $00, $00, $A0, $05, $00, $00, $03, $00, $00, $01, $80, $00, $00, $55, $80, $00, $00, $00, $80,
      $01, $00, $00, $02, $00, $00, $02, $80, $00, $00, $AA, $80, $42, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $80, $02, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $00, $00, $E4, $B0,
      $01, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $00, $00, $E4, $B0, $00, $08, $E4, $A0, $02, $00, $00, $03, $03, $00, $03, $80, $00, $00, $E4, $B1, $01, $00, $E4, $A0, $02, $00, $00, $03,
      $04, $00, $03, $80, $00, $00, $E4, $B1, $02, $00, $E4, $A0, $05, $00, $00, $03, $03, $00, $04, $80, $03, $00, $55, $80, $04, $00, $00, $80, $04, $00, $00, $04, $03, $00, $04, $80, $03, $00, $00, $80,
      $04, $00, $55, $80, $03, $00, $AA, $81, $02, $00, $00, $03, $05, $00, $03, $80, $00, $00, $E4, $B1, $03, $00, $E4, $A0, $05, $00, $00, $03, $03, $00, $08, $80, $04, $00, $55, $80, $05, $00, $00, $80,
      $04, $00, $00, $04, $03, $00, $08, $80, $04, $00, $00, $80, $05, $00, $55, $80, $03, $00, $FF, $81, $05, $00, $00, $03, $03, $00, $04, $80, $03, $00, $AA, $80, $03, $00, $FF, $80, $58, $00, $00, $04,
      $03, $00, $04, $80, $03, $00, $AA, $81, $05, $00, $00, $A0, $05, $00, $55, $A0, $05, $00, $00, $03, $03, $00, $01, $80, $03, $00, $00, $80, $05, $00, $55, $80, $04, $00, $00, $04, $03, $00, $01, $80,
      $05, $00, $00, $80, $03, $00, $55, $80, $03, $00, $00, $81, $05, $00, $00, $03, $03, $00, $01, $80, $03, $00, $FF, $80, $03, $00, $00, $80, $58, $00, $00, $04, $03, $00, $01, $80, $03, $00, $00, $81,
      $05, $00, $00, $A0, $05, $00, $55, $A0, $05, $00, $00, $03, $03, $00, $01, $80, $03, $00, $AA, $80, $03, $00, $00, $80, $58, $00, $00, $04, $01, $00, $0F, $80, $03, $00, $00, $81, $02, $00, $E4, $80,
      $01, $00, $E4, $80, $02, $00, $00, $03, $02, $00, $03, $80, $00, $00, $E4, $B1, $00, $00, $E4, $A0, $05, $00, $00, $03, $02, $00, $04, $80, $04, $00, $00, $80, $02, $00, $55, $80, $04, $00, $00, $04,
      $02, $00, $04, $80, $02, $00, $00, $80, $04, $00, $55, $80, $02, $00, $AA, $81, $05, $00, $00, $03, $02, $00, $04, $80, $03, $00, $FF, $80, $02, $00, $AA, $80, $58, $00, $00, $04, $02, $00, $04, $80,
      $02, $00, $AA, $81, $05, $00, $00, $A0, $05, $00, $55, $A0, $05, $00, $00, $03, $02, $00, $01, $80, $05, $00, $55, $80, $02, $00, $00, $80, $04, $00, $00, $04, $02, $00, $01, $80, $05, $00, $00, $80,
      $02, $00, $55, $80, $02, $00, $00, $81, $05, $00, $00, $03, $02, $00, $01, $80, $03, $00, $FF, $80, $02, $00, $00, $80, $58, $00, $00, $04, $02, $00, $01, $80, $02, $00, $00, $81, $05, $00, $00, $A0,
      $05, $00, $55, $A0, $05, $00, $00, $03, $02, $00, $01, $80, $02, $00, $AA, $80, $02, $00, $00, $80, $58, $00, $00, $04, $00, $00, $0F, $80, $02, $00, $00, $81, $01, $00, $E4, $80, $00, $00, $E4, $80,
      $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('A1Point', TContextShaderVariableKind.Vector, 1, 1),
      TContextShaderVariable.Create('APoint', TContextShaderVariableKind.Vector, 0, 1),
      TContextShaderVariable.Create('BPoint', TContextShaderVariableKind.Vector, 2, 1),
      TContextShaderVariable.Create('Back', TContextShaderVariableKind.Texture, 2, 0),
      TContextShaderVariable.Create('CPoint', TContextShaderVariableKind.Vector, 3, 1),
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('K', TContextShaderVariableKind.Float, 4, 1),
      TContextShaderVariable.Create('Target', TContextShaderVariableKind.Texture, 1, 0)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX11_level_9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $86, $2E, $56, $8F, $0B, $C3, $E4, $4B, $2D, $83, $7A, $82, $7F, $52, $BE, $7C, $01, $00, $00, $00, $98, $0C, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $38, $04, $00, $00,
      $34, $09, $00, $00, $B0, $09, $00, $00, $30, $0C, $00, $00, $64, $0C, $00, $00, $41, $6F, $6E, $39, $F8, $03, $00, $00, $F8, $03, $00, $00, $00, $02, $FF, $FF, $B0, $03, $00, $00, $48, $00, $00, $00,
      $02, $00, $30, $00, $00, $00, $48, $00, $00, $00, $48, $00, $03, $00, $24, $00, $00, $00, $48, $00, $02, $00, $00, $00, $01, $01, $01, $00, $00, $02, $02, $00, $00, $00, $00, $00, $02, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $03, $00, $01, $00, $02, $00, $00, $00, $00, $00, $00, $02, $FF, $FF, $51, $00, $00, $05, $03, $00, $0F, $A0, $00, $00, $00, $00, $00, $00, $00, $80, $00, $00, $80, $BF,
      $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $1F, $00, $00, $02, $00, $00, $00, $90, $01, $08, $0F, $A0,
      $1F, $00, $00, $02, $00, $00, $00, $90, $02, $08, $0F, $A0, $01, $00, $00, $02, $00, $00, $05, $80, $00, $00, $E4, $A0, $05, $00, $00, $03, $01, $00, $02, $80, $00, $00, $00, $80, $02, $00, $00, $A0,
      $01, $00, $00, $02, $01, $00, $04, $80, $00, $00, $55, $A0, $05, $00, $00, $03, $00, $00, $02, $80, $00, $00, $AA, $80, $02, $00, $00, $A0, $01, $00, $00, $02, $00, $00, $04, $80, $00, $00, $FF, $A0,
      $02, $00, $00, $03, $02, $00, $06, $80, $01, $00, $E4, $80, $00, $00, $E4, $81, $5A, $00, $00, $04, $00, $00, $01, $80, $02, $00, $C9, $80, $02, $00, $C9, $80, $03, $00, $00, $A0, $07, $00, $00, $02,
      $00, $00, $01, $80, $00, $00, $00, $80, $05, $00, $00, $03, $02, $00, $06, $80, $02, $00, $E4, $80, $00, $00, $00, $80, $5A, $00, $00, $04, $00, $00, $01, $80, $00, $00, $C9, $80, $02, $00, $C9, $80,
      $03, $00, $00, $A0, $02, $00, $00, $03, $00, $00, $01, $80, $00, $00, $00, $80, $00, $00, $00, $80, $04, $00, $00, $04, $00, $00, $06, $80, $02, $00, $E4, $80, $00, $00, $00, $81, $00, $00, $E4, $80,
      $02, $00, $00, $03, $00, $00, $06, $80, $01, $00, $E4, $81, $00, $00, $E4, $80, $05, $00, $00, $03, $01, $00, $02, $80, $00, $00, $00, $B0, $02, $00, $00, $A0, $01, $00, $00, $02, $01, $00, $04, $80,
      $00, $00, $55, $B0, $5A, $00, $00, $04, $00, $00, $01, $80, $01, $00, $C9, $80, $02, $00, $C9, $80, $03, $00, $00, $A0, $02, $00, $00, $03, $00, $00, $01, $80, $00, $00, $00, $80, $00, $00, $00, $80,
      $04, $00, $00, $04, $01, $00, $06, $80, $02, $00, $E4, $80, $00, $00, $00, $81, $01, $00, $E4, $80, $02, $00, $00, $03, $00, $00, $06, $80, $00, $00, $E4, $81, $01, $00, $E4, $80, $06, $00, $00, $02,
      $00, $00, $08, $80, $02, $00, $00, $A0, $05, $00, $00, $03, $00, $00, $01, $80, $00, $00, $55, $80, $00, $00, $FF, $80, $01, $00, $00, $02, $00, $00, $02, $80, $00, $00, $AA, $80, $42, $00, $00, $03,
      $00, $00, $0F, $80, $00, $00, $E4, $80, $02, $08, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $00, $00, $E4, $B0, $01, $08, $E4, $A0, $42, $00, $00, $03, $02, $00, $0F, $80, $00, $00, $E4, $B0,
      $00, $08, $E4, $A0, $02, $00, $00, $03, $03, $00, $01, $80, $00, $00, $00, $B1, $00, $00, $AA, $A0, $02, $00, $00, $03, $03, $00, $02, $80, $00, $00, $55, $B1, $00, $00, $FF, $A0, $02, $00, $00, $03,
      $04, $00, $01, $80, $00, $00, $00, $B1, $01, $00, $AA, $A0, $02, $00, $00, $03, $04, $00, $02, $80, $00, $00, $55, $B1, $01, $00, $FF, $A0, $05, $00, $00, $03, $03, $00, $04, $80, $03, $00, $00, $80,
      $04, $00, $55, $80, $04, $00, $00, $04, $03, $00, $04, $80, $04, $00, $00, $80, $03, $00, $55, $80, $03, $00, $AA, $81, $02, $00, $00, $03, $04, $00, $0C, $80, $00, $00, $1B, $B1, $01, $00, $1B, $A0,
      $05, $00, $00, $03, $03, $00, $08, $80, $04, $00, $00, $80, $04, $00, $AA, $80, $04, $00, $00, $04, $03, $00, $08, $80, $04, $00, $FF, $80, $04, $00, $55, $80, $03, $00, $FF, $81, $05, $00, $00, $03,
      $03, $00, $04, $80, $03, $00, $AA, $80, $03, $00, $FF, $80, $58, $00, $00, $04, $03, $00, $04, $80, $03, $00, $AA, $81, $03, $00, $55, $A0, $03, $00, $AA, $A0, $05, $00, $00, $03, $03, $00, $02, $80,
      $03, $00, $55, $80, $04, $00, $FF, $80, $04, $00, $00, $04, $03, $00, $01, $80, $03, $00, $00, $80, $04, $00, $AA, $80, $03, $00, $55, $81, $05, $00, $00, $03, $03, $00, $01, $80, $03, $00, $FF, $80,
      $03, $00, $00, $80, $58, $00, $00, $04, $03, $00, $01, $80, $03, $00, $00, $81, $03, $00, $55, $A0, $03, $00, $AA, $80, $58, $00, $00, $04, $01, $00, $0F, $80, $03, $00, $00, $80, $02, $00, $E4, $80,
      $01, $00, $E4, $80, $02, $00, $00, $03, $02, $00, $03, $80, $00, $00, $E4, $B1, $00, $00, $E4, $A0, $05, $00, $00, $03, $02, $00, $04, $80, $04, $00, $55, $80, $02, $00, $00, $80, $04, $00, $00, $04,
      $02, $00, $04, $80, $04, $00, $00, $80, $02, $00, $55, $80, $02, $00, $AA, $81, $05, $00, $00, $03, $02, $00, $04, $80, $03, $00, $FF, $80, $02, $00, $AA, $80, $58, $00, $00, $04, $02, $00, $04, $80,
      $02, $00, $AA, $81, $03, $00, $55, $A0, $03, $00, $AA, $A0, $05, $00, $00, $03, $02, $00, $02, $80, $04, $00, $FF, $80, $02, $00, $55, $80, $04, $00, $00, $04, $02, $00, $01, $80, $02, $00, $00, $80,
      $04, $00, $AA, $80, $02, $00, $55, $81, $05, $00, $00, $03, $02, $00, $01, $80, $03, $00, $FF, $80, $02, $00, $00, $80, $58, $00, $00, $04, $02, $00, $01, $80, $02, $00, $00, $81, $03, $00, $55, $A0,
      $02, $00, $AA, $80, $58, $00, $00, $04, $00, $00, $0F, $80, $02, $00, $00, $80, $01, $00, $E4, $80, $00, $00, $E4, $80, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00,
      $53, $48, $44, $52, $F4, $04, $00, $00, $40, $00, $00, $00, $3D, $01, $00, $00, $59, $00, $00, $04, $46, $8E, $20, $00, $00, $00, $00, $00, $04, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00,
      $00, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $01, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $02, $00, $00, $00, $58, $18, $00, $04, $00, $70, $10, $00, $00, $00, $00, $00,
      $55, $55, $00, $00, $58, $18, $00, $04, $00, $70, $10, $00, $01, $00, $00, $00, $55, $55, $00, $00, $58, $18, $00, $04, $00, $70, $10, $00, $02, $00, $00, $00, $55, $55, $00, $00, $62, $10, $00, $03,
      $32, $10, $10, $00, $00, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00, $00, $00, $00, $00, $68, $00, $00, $02, $04, $00, $00, $00, $00, $00, $00, $09, $F2, $00, $10, $00, $00, $00, $00, $00,
      $46, $11, $10, $80, $41, $00, $00, $00, $00, $00, $00, $00, $46, $81, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $09, $F2, $00, $10, $00, $01, $00, $00, $00, $16, $14, $10, $80,
      $41, $00, $00, $00, $00, $00, $00, $00, $16, $8E, $20, $00, $00, $00, $00, $00, $01, $00, $00, $00, $38, $00, $00, $07, $A2, $00, $10, $00, $00, $00, $00, $00, $56, $0D, $10, $00, $00, $00, $00, $00,
      $56, $0D, $10, $00, $01, $00, $00, $00, $32, $00, $00, $0A, $32, $00, $10, $00, $00, $00, $00, $00, $86, $00, $10, $00, $00, $00, $00, $00, $86, $00, $10, $00, $01, $00, $00, $00, $D6, $05, $10, $80,
      $41, $00, $00, $00, $00, $00, $00, $00, $38, $00, $00, $07, $42, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00, $01, $00, $00, $00, $2A, $00, $10, $00, $01, $00, $00, $00, $32, $00, $00, $0A,
      $42, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00, $01, $00, $00, $00, $3A, $00, $10, $00, $01, $00, $00, $00, $2A, $00, $10, $80, $41, $00, $00, $00, $00, $00, $00, $00, $38, $00, $00, $07,
      $32, $00, $10, $00, $00, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $A6, $0A, $10, $00, $00, $00, $00, $00, $31, $00, $00, $0A, $32, $00, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $01, $00, $00, $07, $12, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00,
      $00, $00, $00, $00, $0A, $00, $10, $00, $00, $00, $00, $00, $38, $00, $00, $09, $52, $00, $10, $00, $02, $00, $00, $00, $A6, $88, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $80, $20, $00,
      $00, $00, $00, $00, $03, $00, $00, $00, $38, $00, $00, $08, $22, $00, $10, $00, $03, $00, $00, $00, $0A, $10, $10, $00, $00, $00, $00, $00, $0A, $80, $20, $00, $00, $00, $00, $00, $03, $00, $00, $00,
      $36, $00, $00, $06, $A2, $00, $10, $00, $02, $00, $00, $00, $F6, $87, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $08, $A2, $00, $10, $00, $00, $00, $00, $00, $06, $04, $10, $80,
      $41, $00, $00, $00, $02, $00, $00, $00, $A6, $0E, $10, $00, $02, $00, $00, $00, $0F, $00, $00, $07, $12, $00, $10, $00, $03, $00, $00, $00, $D6, $05, $10, $00, $00, $00, $00, $00, $D6, $05, $10, $00,
      $00, $00, $00, $00, $44, $00, $00, $05, $12, $00, $10, $00, $03, $00, $00, $00, $0A, $00, $10, $00, $03, $00, $00, $00, $38, $00, $00, $07, $A2, $00, $10, $00, $00, $00, $00, $00, $56, $0D, $10, $00,
      $00, $00, $00, $00, $06, $00, $10, $00, $03, $00, $00, $00, $36, $00, $00, $05, $42, $00, $10, $00, $03, $00, $00, $00, $1A, $10, $10, $00, $00, $00, $00, $00, $11, $00, $00, $07, $12, $00, $10, $00,
      $03, $00, $00, $00, $56, $0A, $10, $00, $03, $00, $00, $00, $56, $0F, $10, $00, $00, $00, $00, $00, $32, $00, $00, $0A, $32, $00, $10, $00, $03, $00, $00, $00, $D6, $05, $10, $00, $00, $00, $00, $00,
      $06, $00, $10, $80, $41, $00, $00, $00, $03, $00, $00, $00, $96, $05, $10, $00, $03, $00, $00, $00, $11, $00, $00, $07, $42, $00, $10, $00, $03, $00, $00, $00, $06, $05, $10, $00, $02, $00, $00, $00,
      $56, $0F, $10, $00, $00, $00, $00, $00, $32, $00, $00, $0A, $A2, $00, $10, $00, $00, $00, $00, $00, $56, $0D, $10, $00, $00, $00, $00, $00, $A6, $0A, $10, $80, $41, $00, $00, $00, $03, $00, $00, $00,
      $06, $04, $10, $00, $02, $00, $00, $00, $00, $00, $00, $08, $A2, $00, $10, $00, $00, $00, $00, $00, $A6, $0E, $10, $80, $41, $00, $00, $00, $02, $00, $00, $00, $56, $0D, $10, $00, $00, $00, $00, $00,
      $00, $00, $00, $08, $62, $00, $10, $00, $02, $00, $00, $00, $06, $01, $10, $00, $03, $00, $00, $00, $56, $07, $10, $80, $41, $00, $00, $00, $00, $00, $00, $00, $0E, $00, $00, $08, $12, $00, $10, $00,
      $02, $00, $00, $00, $1A, $00, $10, $00, $02, $00, $00, $00, $0A, $80, $20, $00, $00, $00, $00, $00, $03, $00, $00, $00, $45, $00, $00, $09, $F2, $00, $10, $00, $02, $00, $00, $00, $86, $00, $10, $00,
      $02, $00, $00, $00, $46, $7E, $10, $00, $00, $00, $00, $00, $00, $60, $10, $00, $02, $00, $00, $00, $1F, $00, $04, $03, $0A, $00, $10, $00, $00, $00, $00, $00, $36, $00, $00, $05, $F2, $20, $10, $00,
      $00, $00, $00, $00, $46, $0E, $10, $00, $02, $00, $00, $00, $12, $00, $00, $01, $00, $00, $00, $09, $F2, $00, $10, $00, $02, $00, $00, $00, $46, $11, $10, $80, $41, $00, $00, $00, $00, $00, $00, $00,
      $E6, $8B, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $38, $00, $00, $07, $32, $00, $10, $00, $00, $00, $00, $00, $D6, $05, $10, $00, $01, $00, $00, $00, $D6, $05, $10, $00, $02, $00, $00, $00,
      $32, $00, $00, $0A, $32, $00, $10, $00, $00, $00, $00, $00, $86, $00, $10, $00, $02, $00, $00, $00, $86, $00, $10, $00, $01, $00, $00, $00, $46, $00, $10, $80, $41, $00, $00, $00, $00, $00, $00, $00,
      $38, $00, $00, $07, $32, $00, $10, $00, $00, $00, $00, $00, $A6, $0A, $10, $00, $00, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $31, $00, $00, $0A, $32, $00, $10, $00, $00, $00, $00, $00,
      $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $01, $00, $00, $07, $12, $00, $10, $00, $00, $00, $00, $00,
      $1A, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00, $00, $00, $00, $00, $1F, $00, $04, $03, $0A, $00, $10, $00, $00, $00, $00, $00, $45, $00, $00, $09, $F2, $20, $10, $00, $00, $00, $00, $00,
      $46, $10, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00, $01, $00, $00, $00, $00, $60, $10, $00, $01, $00, $00, $00, $12, $00, $00, $01, $45, $00, $00, $09, $F2, $20, $10, $00, $00, $00, $00, $00,
      $46, $10, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00, $02, $00, $00, $00, $00, $60, $10, $00, $00, $00, $00, $00, $15, $00, $00, $01, $15, $00, $00, $01, $3E, $00, $00, $01, $53, $54, $41, $54,
      $74, $00, $00, $00, $29, $00, $00, $00, $04, $00, $00, $00, $00, $00, $00, $00, $02, $00, $00, $00, $15, $00, $00, $00, $00, $00, $00, $00, $02, $00, $00, $00, $03, $00, $00, $00, $02, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $52, $44, $45, $46, $78, $02, $00, $00, $01, $00, $00, $00, $18, $01, $00, $00, $07, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FF, $FF, $00, $11, $00, $00, $44, $02, $00, $00, $FC, $00, $00, $00,
      $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $01, $00, $00, $00, $02, $01, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $01, $00, $00, $00, $01, $00, $00, $00, $09, $01, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $02, $00, $00, $00, $01, $00, $00, $00, $01, $00, $00, $00, $09, $01, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $01, $00, $00, $00,
      $0C, $00, $00, $00, $02, $01, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $01, $00, $00, $00, $01, $00, $00, $00, $0C, $00, $00, $00, $FC, $00, $00, $00,
      $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $02, $00, $00, $00, $01, $00, $00, $00, $0C, $00, $00, $00, $0E, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $49, $6E, $70, $75, $74, $00, $54, $61, $72, $67, $65, $74, $00, $42, $61, $63, $6B, $00, $24, $47,
      $6C, $6F, $62, $61, $6C, $73, $00, $AB, $0E, $01, $00, $00, $08, $00, $00, $00, $30, $01, $00, $00, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $F0, $01, $00, $00, $00, $00, $00, $00,
      $08, $00, $00, $00, $02, $00, $00, $00, $F8, $01, $00, $00, $00, $00, $00, $00, $08, $02, $00, $00, $08, $00, $00, $00, $08, $00, $00, $00, $02, $00, $00, $00, $F8, $01, $00, $00, $00, $00, $00, $00,
      $10, $02, $00, $00, $10, $00, $00, $00, $08, $00, $00, $00, $02, $00, $00, $00, $F8, $01, $00, $00, $00, $00, $00, $00, $17, $02, $00, $00, $18, $00, $00, $00, $08, $00, $00, $00, $02, $00, $00, $00,
      $F8, $01, $00, $00, $00, $00, $00, $00, $1E, $02, $00, $00, $20, $00, $00, $00, $08, $00, $00, $00, $00, $00, $00, $00, $F8, $01, $00, $00, $00, $00, $00, $00, $25, $02, $00, $00, $28, $00, $00, $00,
      $08, $00, $00, $00, $00, $00, $00, $00, $F8, $01, $00, $00, $00, $00, $00, $00, $2D, $02, $00, $00, $30, $00, $00, $00, $04, $00, $00, $00, $02, $00, $00, $00, $30, $02, $00, $00, $00, $00, $00, $00,
      $40, $02, $00, $00, $34, $00, $00, $00, $04, $00, $00, $00, $00, $00, $00, $00, $30, $02, $00, $00, $00, $00, $00, $00, $41, $50, $6F, $69, $6E, $74, $00, $AB, $01, $00, $03, $00, $01, $00, $02, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $41, $31, $50, $6F, $69, $6E, $74, $00, $42, $50, $6F, $69, $6E, $74, $00, $43, $50, $6F, $69, $6E, $74, $00, $4C, $69, $6E, $65, $41, $42, $00, $4C, $69, $6E,
      $65, $41, $42, $31, $00, $4B, $00, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $4C, $65, $6E, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52,
      $29, $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E, $39, $35, $32, $2E, $32, $38, $34, $34, $00, $AB, $AB, $AB,
      $49, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $03, $03, $00, $00,
      $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $AB, $AB, $AB, $4F, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $53, $56, $5F, $54, $61, $72, $67, $65, $74, $00, $AB, $AB], [
      TContextShaderVariable.Create('Back', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Target', TContextShaderVariableKind.Texture, 1, 0),
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 2, 0),
      TContextShaderVariable.Create('APoint', TContextShaderVariableKind.Vector, 0, 8),
      TContextShaderVariable.Create('A1Point', TContextShaderVariableKind.Vector, 8, 8),
      TContextShaderVariable.Create('BPoint', TContextShaderVariableKind.Vector, 16, 8),
      TContextShaderVariable.Create('CPoint', TContextShaderVariableKind.Vector, 24, 8),
      TContextShaderVariable.Create('LineAB', TContextShaderVariableKind.Vector, 32, 8),
      TContextShaderVariable.Create('LineAB1', TContextShaderVariableKind.Vector, 40, 8),
      TContextShaderVariable.Create('K', TContextShaderVariableKind.Float, 48, 4),
      TContextShaderVariable.Create('Len', TContextShaderVariableKind.Float, 52, 4)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX10'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX10, [
      $44, $58, $42, $43, $D2, $D8, $C1, $78, $45, $7F, $7A, $C3, $0F, $EB, $B0, $F8, $CA, $9A, $7C, $CC, $01, $00, $00, $00, $A0, $12, $00, $00, $05, $00, $00, $00, $34, $00, $00, $00, $B4, $02, $00, $00,
      $E8, $02, $00, $00, $1C, $03, $00, $00, $24, $12, $00, $00, $52, $44, $45, $46, $78, $02, $00, $00, $01, $00, $00, $00, $18, $01, $00, $00, $07, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FF, $FF,
      $00, $11, $00, $00, $44, $02, $00, $00, $FC, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $01, $00, $00, $00,
      $02, $01, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $01, $00, $00, $00, $01, $00, $00, $00, $09, $01, $00, $00, $03, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $02, $00, $00, $00, $01, $00, $00, $00, $01, $00, $00, $00, $FC, $00, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00,
      $FF, $FF, $FF, $FF, $00, $00, $00, $00, $01, $00, $00, $00, $0C, $00, $00, $00, $02, $01, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $01, $00, $00, $00,
      $01, $00, $00, $00, $0C, $00, $00, $00, $09, $01, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $02, $00, $00, $00, $01, $00, $00, $00, $0C, $00, $00, $00,
      $0E, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $49, $6E, $70, $75, $74, $00, $54, $61,
      $72, $67, $65, $74, $00, $42, $61, $63, $6B, $00, $24, $47, $6C, $6F, $62, $61, $6C, $73, $00, $AB, $0E, $01, $00, $00, $08, $00, $00, $00, $30, $01, $00, $00, $40, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $F0, $01, $00, $00, $00, $00, $00, $00, $08, $00, $00, $00, $02, $00, $00, $00, $F8, $01, $00, $00, $00, $00, $00, $00, $08, $02, $00, $00, $08, $00, $00, $00, $08, $00, $00, $00,
      $02, $00, $00, $00, $F8, $01, $00, $00, $00, $00, $00, $00, $10, $02, $00, $00, $10, $00, $00, $00, $08, $00, $00, $00, $02, $00, $00, $00, $F8, $01, $00, $00, $00, $00, $00, $00, $17, $02, $00, $00,
      $18, $00, $00, $00, $08, $00, $00, $00, $02, $00, $00, $00, $F8, $01, $00, $00, $00, $00, $00, $00, $1E, $02, $00, $00, $20, $00, $00, $00, $08, $00, $00, $00, $02, $00, $00, $00, $F8, $01, $00, $00,
      $00, $00, $00, $00, $25, $02, $00, $00, $28, $00, $00, $00, $08, $00, $00, $00, $02, $00, $00, $00, $F8, $01, $00, $00, $00, $00, $00, $00, $2D, $02, $00, $00, $30, $00, $00, $00, $04, $00, $00, $00,
      $02, $00, $00, $00, $30, $02, $00, $00, $00, $00, $00, $00, $40, $02, $00, $00, $34, $00, $00, $00, $04, $00, $00, $00, $02, $00, $00, $00, $30, $02, $00, $00, $00, $00, $00, $00, $41, $50, $6F, $69,
      $6E, $74, $00, $AB, $01, $00, $03, $00, $01, $00, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00, $41, $31, $50, $6F, $69, $6E, $74, $00, $42, $50, $6F, $69, $6E, $74, $00, $43, $50, $6F, $69, $6E,
      $74, $00, $4C, $69, $6E, $65, $41, $42, $00, $4C, $69, $6E, $65, $41, $42, $31, $00, $4B, $00, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $4C, $65, $6E, $00,
      $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E,
      $39, $35, $32, $2E, $32, $38, $34, $34, $00, $AB, $AB, $AB, $49, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $03, $00, $00, $00, $00, $00, $00, $00, $03, $03, $00, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $AB, $AB, $AB, $4F, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00,
      $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $53, $56, $5F, $54, $61, $72, $67, $65, $74, $00, $AB, $AB, $53, $48, $44, $52,
      $00, $0F, $00, $00, $40, $00, $00, $00, $C0, $03, $00, $00, $59, $00, $00, $04, $46, $8E, $20, $00, $00, $00, $00, $00, $04, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $00, $00, $00, $00,
      $5A, $00, $00, $03, $00, $60, $10, $00, $01, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $02, $00, $00, $00, $58, $18, $00, $04, $00, $70, $10, $00, $00, $00, $00, $00, $55, $55, $00, $00,
      $58, $18, $00, $04, $00, $70, $10, $00, $01, $00, $00, $00, $55, $55, $00, $00, $58, $18, $00, $04, $00, $70, $10, $00, $02, $00, $00, $00, $55, $55, $00, $00, $62, $10, $00, $03, $32, $10, $10, $00,
      $00, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00, $00, $00, $00, $00, $68, $00, $00, $02, $07, $00, $00, $00, $32, $00, $00, $0B, $32, $00, $10, $00, $00, $00, $00, $00, $26, $8A, $20, $00,
      $00, $00, $00, $00, $02, $00, $00, $00, $06, $10, $10, $00, $00, $00, $00, $00, $76, $8F, $20, $00, $00, $00, $00, $00, $02, $00, $00, $00, $1D, $00, $00, $07, $C2, $00, $10, $00, $00, $00, $00, $00,
      $56, $15, $10, $00, $00, $00, $00, $00, $56, $01, $10, $00, $00, $00, $00, $00, $1D, $00, $00, $07, $32, $00, $10, $00, $01, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $56, $15, $10, $00,
      $00, $00, $00, $00, $01, $00, $00, $07, $62, $00, $10, $00, $00, $00, $00, $00, $A6, $0B, $10, $00, $00, $00, $00, $00, $06, $01, $10, $00, $01, $00, $00, $00, $3C, $00, $00, $07, $22, $00, $10, $00,
      $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00, $00, $00, $00, $00, $00, $00, $00, $09, $42, $00, $10, $00, $00, $00, $00, $00, $1A, $10, $10, $00, $00, $00, $00, $00,
      $3A, $80, $20, $80, $41, $00, $00, $00, $00, $00, $00, $00, $02, $00, $00, $00, $0E, $00, $00, $08, $42, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $2A, $80, $20, $00,
      $00, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $0A, $F2, $00, $10, $00, $01, $00, $00, $00, $E6, $84, $20, $80, $41, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $46, $8E, $20, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $07, $82, $00, $10, $00, $00, $00, $00, $00, $46, $00, $10, $00, $01, $00, $00, $00, $46, $00, $10, $00, $01, $00, $00, $00, $4B, $00, $00, $05,
      $82, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00, $00, $00, $00, $00, $00, $00, $00, $0A, $12, $00, $10, $00, $01, $00, $00, $00, $2A, $80, $20, $80, $41, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $2A, $80, $20, $00, $00, $00, $00, $00, $01, $00, $00, $00, $0E, $00, $00, $07, $82, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00,
      $01, $00, $00, $00, $00, $00, $00, $08, $42, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $0A, $10, $10, $80, $41, $00, $00, $00, $00, $00, $00, $00, $38, $00, $00, $07,
      $42, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $0E, $00, $00, $08, $82, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00,
      $00, $00, $00, $00, $1A, $80, $20, $00, $00, $00, $00, $00, $03, $00, $00, $00, $38, $00, $00, $07, $82, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00,
      $00, $00, $00, $00, $0E, $00, $00, $08, $82, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00, $00, $00, $00, $00, $1A, $80, $20, $00, $00, $00, $00, $00, $03, $00, $00, $00, $38, $00, $00, $07,
      $82, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00, $00, $00, $00, $00, $0E, $00, $00, $08, $82, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00,
      $00, $00, $00, $00, $1A, $80, $20, $00, $00, $00, $00, $00, $03, $00, $00, $00, $38, $00, $00, $07, $42, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00,
      $00, $00, $00, $00, $38, $00, $00, $07, $42, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $01, $40, $00, $00, $CD, $CC, $CC, $3D, $0F, $00, $00, $07, $82, $00, $10, $00,
      $00, $00, $00, $00, $E6, $0A, $10, $00, $01, $00, $00, $00, $E6, $0A, $10, $00, $01, $00, $00, $00, $44, $00, $00, $05, $82, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00, $00, $00, $00, $00,
      $38, $00, $00, $07, $32, $00, $10, $00, $01, $00, $00, $00, $E6, $0A, $10, $00, $01, $00, $00, $00, $F6, $0F, $10, $00, $00, $00, $00, $00, $32, $00, $00, $09, $62, $00, $10, $00, $01, $00, $00, $00,
      $A6, $0A, $10, $00, $00, $00, $00, $00, $06, $01, $10, $00, $01, $00, $00, $00, $06, $11, $10, $00, $00, $00, $00, $00, $38, $00, $00, $09, $52, $00, $10, $00, $02, $00, $00, $00, $A6, $88, $20, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $06, $80, $20, $00, $00, $00, $00, $00, $03, $00, $00, $00, $38, $00, $00, $08, $22, $00, $10, $00, $03, $00, $00, $00, $0A, $10, $10, $00, $00, $00, $00, $00,
      $0A, $80, $20, $00, $00, $00, $00, $00, $03, $00, $00, $00, $36, $00, $00, $06, $A2, $00, $10, $00, $02, $00, $00, $00, $F6, $87, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $08,
      $C2, $00, $10, $00, $00, $00, $00, $00, $06, $04, $10, $80, $41, $00, $00, $00, $02, $00, $00, $00, $A6, $0E, $10, $00, $02, $00, $00, $00, $0F, $00, $00, $07, $82, $00, $10, $00, $01, $00, $00, $00,
      $E6, $0A, $10, $00, $00, $00, $00, $00, $E6, $0A, $10, $00, $00, $00, $00, $00, $44, $00, $00, $05, $82, $00, $10, $00, $01, $00, $00, $00, $3A, $00, $10, $00, $01, $00, $00, $00, $38, $00, $00, $07,
      $C2, $00, $10, $00, $00, $00, $00, $00, $A6, $0E, $10, $00, $00, $00, $00, $00, $F6, $0F, $10, $00, $01, $00, $00, $00, $36, $00, $00, $05, $42, $00, $10, $00, $03, $00, $00, $00, $1A, $10, $10, $00,
      $00, $00, $00, $00, $11, $00, $00, $07, $82, $00, $10, $00, $01, $00, $00, $00, $56, $0A, $10, $00, $03, $00, $00, $00, $A6, $0F, $10, $00, $00, $00, $00, $00, $32, $00, $00, $0A, $32, $00, $10, $00,
      $03, $00, $00, $00, $E6, $0A, $10, $00, $00, $00, $00, $00, $F6, $0F, $10, $80, $41, $00, $00, $00, $01, $00, $00, $00, $96, $05, $10, $00, $03, $00, $00, $00, $11, $00, $00, $07, $82, $00, $10, $00,
      $01, $00, $00, $00, $06, $05, $10, $00, $02, $00, $00, $00, $A6, $0F, $10, $00, $00, $00, $00, $00, $32, $00, $00, $0A, $32, $00, $10, $00, $02, $00, $00, $00, $E6, $0A, $10, $00, $00, $00, $00, $00,
      $F6, $0F, $10, $80, $41, $00, $00, $00, $01, $00, $00, $00, $46, $00, $10, $00, $02, $00, $00, $00, $00, $00, $00, $08, $32, $00, $10, $00, $02, $00, $00, $00, $E6, $0A, $10, $80, $41, $00, $00, $00,
      $02, $00, $00, $00, $46, $00, $10, $00, $02, $00, $00, $00, $00, $00, $00, $08, $62, $00, $10, $00, $03, $00, $00, $00, $06, $01, $10, $00, $03, $00, $00, $00, $06, $01, $10, $80, $41, $00, $00, $00,
      $02, $00, $00, $00, $0E, $00, $00, $08, $12, $00, $10, $00, $03, $00, $00, $00, $1A, $00, $10, $00, $03, $00, $00, $00, $0A, $80, $20, $00, $00, $00, $00, $00, $03, $00, $00, $00, $45, $00, $00, $09,
      $F2, $00, $10, $00, $03, $00, $00, $00, $86, $00, $10, $00, $03, $00, $00, $00, $46, $7E, $10, $00, $02, $00, $00, $00, $00, $60, $10, $00, $02, $00, $00, $00, $38, $00, $00, $08, $12, $00, $10, $00,
      $01, $00, $00, $00, $1A, $00, $10, $00, $01, $00, $00, $00, $0A, $80, $20, $00, $00, $00, $00, $00, $03, $00, $00, $00, $11, $00, $00, $07, $82, $00, $10, $00, $01, $00, $00, $00, $06, $0A, $10, $00,
      $01, $00, $00, $00, $A6, $0F, $10, $00, $00, $00, $00, $00, $32, $00, $00, $0A, $C2, $00, $10, $00, $00, $00, $00, $00, $A6, $0E, $10, $00, $00, $00, $00, $00, $F6, $0F, $10, $80, $41, $00, $00, $00,
      $01, $00, $00, $00, $06, $08, $10, $00, $01, $00, $00, $00, $00, $00, $00, $08, $62, $00, $10, $00, $02, $00, $00, $00, $06, $01, $10, $80, $41, $00, $00, $00, $02, $00, $00, $00, $A6, $0B, $10, $00,
      $00, $00, $00, $00, $0E, $00, $00, $08, $12, $00, $10, $00, $02, $00, $00, $00, $1A, $00, $10, $00, $02, $00, $00, $00, $0A, $80, $20, $00, $00, $00, $00, $00, $03, $00, $00, $00, $45, $00, $00, $09,
      $F2, $00, $10, $00, $02, $00, $00, $00, $86, $00, $10, $00, $02, $00, $00, $00, $46, $7E, $10, $00, $02, $00, $00, $00, $00, $60, $10, $00, $02, $00, $00, $00, $45, $00, $00, $09, $F2, $00, $10, $00,
      $04, $00, $00, $00, $96, $05, $10, $00, $01, $00, $00, $00, $46, $7E, $10, $00, $00, $00, $00, $00, $00, $60, $10, $00, $00, $00, $00, $00, $1F, $00, $04, $03, $1A, $00, $10, $00, $00, $00, $00, $00,
      $31, $00, $00, $0A, $62, $00, $10, $00, $00, $00, $00, $00, $56, $06, $10, $00, $01, $00, $00, $00, $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $3C, $00, $00, $07, $22, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00, $00, $00, $00, $00, $31, $00, $00, $0A, $C2, $00, $10, $00, $00, $00, $00, $00,
      $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $80, $3F, $00, $00, $80, $3F, $56, $09, $10, $00, $01, $00, $00, $00, $3C, $00, $00, $07, $22, $00, $10, $00, $00, $00, $00, $00,
      $1A, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $3C, $00, $00, $07, $22, $00, $10, $00, $00, $00, $00, $00, $3A, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00,
      $00, $00, $00, $00, $1F, $00, $04, $03, $1A, $00, $10, $00, $00, $00, $00, $00, $45, $00, $00, $09, $F2, $20, $10, $00, $00, $00, $00, $00, $46, $10, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00,
      $01, $00, $00, $00, $00, $60, $10, $00, $01, $00, $00, $00, $12, $00, $00, $01, $00, $00, $00, $09, $F2, $00, $10, $00, $05, $00, $00, $00, $96, $06, $10, $80, $41, $00, $00, $00, $01, $00, $00, $00,
      $46, $81, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $09, $F2, $00, $10, $00, $06, $00, $00, $00, $66, $09, $10, $80, $41, $00, $00, $00, $01, $00, $00, $00, $16, $8E, $20, $00,
      $00, $00, $00, $00, $01, $00, $00, $00, $38, $00, $00, $07, $62, $00, $10, $00, $00, $00, $00, $00, $56, $07, $10, $00, $05, $00, $00, $00, $56, $07, $10, $00, $06, $00, $00, $00, $32, $00, $00, $0A,
      $62, $00, $10, $00, $00, $00, $00, $00, $06, $02, $10, $00, $05, $00, $00, $00, $06, $02, $10, $00, $06, $00, $00, $00, $56, $06, $10, $80, $41, $00, $00, $00, $00, $00, $00, $00, $38, $00, $00, $07,
      $82, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00, $06, $00, $00, $00, $2A, $00, $10, $00, $06, $00, $00, $00, $32, $00, $00, $0A, $82, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00,
      $06, $00, $00, $00, $3A, $00, $10, $00, $06, $00, $00, $00, $3A, $00, $10, $80, $41, $00, $00, $00, $00, $00, $00, $00, $38, $00, $00, $07, $62, $00, $10, $00, $00, $00, $00, $00, $56, $06, $10, $00,
      $00, $00, $00, $00, $F6, $0F, $10, $00, $00, $00, $00, $00, $31, $00, $00, $0A, $62, $00, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $56, $06, $10, $00, $00, $00, $00, $00, $01, $00, $00, $07, $22, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00, $00, $00, $00, $00,
      $1F, $00, $04, $03, $1A, $00, $10, $00, $00, $00, $00, $00, $36, $00, $00, $05, $F2, $20, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $02, $00, $00, $00, $12, $00, $00, $01, $00, $00, $00, $09,
      $F2, $00, $10, $00, $01, $00, $00, $00, $96, $06, $10, $80, $41, $00, $00, $00, $01, $00, $00, $00, $E6, $8B, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $38, $00, $00, $07, $62, $00, $10, $00,
      $00, $00, $00, $00, $56, $07, $10, $00, $06, $00, $00, $00, $56, $07, $10, $00, $01, $00, $00, $00, $32, $00, $00, $0A, $62, $00, $10, $00, $00, $00, $00, $00, $06, $02, $10, $00, $01, $00, $00, $00,
      $06, $02, $10, $00, $06, $00, $00, $00, $56, $06, $10, $80, $41, $00, $00, $00, $00, $00, $00, $00, $38, $00, $00, $07, $62, $00, $10, $00, $00, $00, $00, $00, $F6, $0F, $10, $00, $00, $00, $00, $00,
      $56, $06, $10, $00, $00, $00, $00, $00, $31, $00, $00, $0A, $62, $00, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $56, $06, $10, $00, $00, $00, $00, $00, $01, $00, $00, $07, $22, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00, $00, $00, $00, $00, $1F, $00, $04, $03,
      $1A, $00, $10, $00, $00, $00, $00, $00, $45, $00, $00, $09, $F2, $20, $10, $00, $00, $00, $00, $00, $46, $10, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00, $01, $00, $00, $00, $00, $60, $10, $00,
      $01, $00, $00, $00, $12, $00, $00, $01, $36, $00, $00, $05, $F2, $20, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $04, $00, $00, $00, $15, $00, $00, $01, $15, $00, $00, $01, $15, $00, $00, $01,
      $12, $00, $00, $01, $00, $00, $00, $09, $F2, $00, $10, $00, $01, $00, $00, $00, $46, $11, $10, $80, $41, $00, $00, $00, $00, $00, $00, $00, $46, $81, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $09, $F2, $00, $10, $00, $02, $00, $00, $00, $16, $14, $10, $80, $41, $00, $00, $00, $00, $00, $00, $00, $16, $8E, $20, $00, $00, $00, $00, $00, $01, $00, $00, $00, $38, $00, $00, $07,
      $62, $00, $10, $00, $00, $00, $00, $00, $56, $07, $10, $00, $01, $00, $00, $00, $56, $07, $10, $00, $02, $00, $00, $00, $32, $00, $00, $0A, $62, $00, $10, $00, $00, $00, $00, $00, $06, $02, $10, $00,
      $01, $00, $00, $00, $06, $02, $10, $00, $02, $00, $00, $00, $56, $06, $10, $80, $41, $00, $00, $00, $00, $00, $00, $00, $38, $00, $00, $07, $82, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00,
      $02, $00, $00, $00, $2A, $00, $10, $00, $02, $00, $00, $00, $32, $00, $00, $0A, $82, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00, $02, $00, $00, $00, $3A, $00, $10, $00, $02, $00, $00, $00,
      $3A, $00, $10, $80, $41, $00, $00, $00, $00, $00, $00, $00, $38, $00, $00, $07, $62, $00, $10, $00, $00, $00, $00, $00, $56, $06, $10, $00, $00, $00, $00, $00, $F6, $0F, $10, $00, $00, $00, $00, $00,
      $31, $00, $00, $0A, $62, $00, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $56, $06, $10, $00, $00, $00, $00, $00,
      $01, $00, $00, $07, $22, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00, $00, $00, $00, $00, $1F, $00, $04, $03, $1A, $00, $10, $00, $00, $00, $00, $00,
      $36, $00, $00, $05, $F2, $20, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $03, $00, $00, $00, $12, $00, $00, $01, $00, $00, $00, $09, $F2, $00, $10, $00, $01, $00, $00, $00, $46, $11, $10, $80,
      $41, $00, $00, $00, $00, $00, $00, $00, $E6, $8B, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $38, $00, $00, $07, $62, $00, $10, $00, $00, $00, $00, $00, $56, $07, $10, $00, $02, $00, $00, $00,
      $56, $07, $10, $00, $01, $00, $00, $00, $32, $00, $00, $0A, $62, $00, $10, $00, $00, $00, $00, $00, $06, $02, $10, $00, $01, $00, $00, $00, $06, $02, $10, $00, $02, $00, $00, $00, $56, $06, $10, $80,
      $41, $00, $00, $00, $00, $00, $00, $00, $38, $00, $00, $07, $62, $00, $10, $00, $00, $00, $00, $00, $F6, $0F, $10, $00, $00, $00, $00, $00, $56, $06, $10, $00, $00, $00, $00, $00, $31, $00, $00, $0A,
      $62, $00, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $56, $06, $10, $00, $00, $00, $00, $00, $01, $00, $00, $07,
      $22, $00, $10, $00, $00, $00, $00, $00, $2A, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00, $00, $00, $00, $00, $1F, $00, $04, $03, $1A, $00, $10, $00, $00, $00, $00, $00, $45, $00, $00, $09,
      $F2, $20, $10, $00, $00, $00, $00, $00, $46, $10, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00, $01, $00, $00, $00, $00, $60, $10, $00, $01, $00, $00, $00, $12, $00, $00, $01, $45, $00, $00, $09,
      $F2, $20, $10, $00, $00, $00, $00, $00, $46, $10, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00, $00, $00, $00, $00, $00, $60, $10, $00, $00, $00, $00, $00, $15, $00, $00, $01, $15, $00, $00, $01,
      $15, $00, $00, $01, $00, $00, $00, $08, $12, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00, $00, $00, $00, $00, $1A, $10, $10, $80, $41, $00, $00, $00, $00, $00, $00, $00, $31, $00, $00, $08,
      $12, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $80, $81, $00, $00, $00, $00, $00, $00, $00, $01, $40, $00, $00, $6F, $12, $83, $3A, $1F, $00, $04, $03, $0A, $00, $10, $00, $00, $00, $00, $00,
      $00, $00, $00, $09, $F2, $00, $10, $00, $00, $00, $00, $00, $46, $11, $10, $80, $41, $00, $00, $00, $00, $00, $00, $00, $46, $81, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $09,
      $F2, $00, $10, $00, $01, $00, $00, $00, $16, $14, $10, $80, $41, $00, $00, $00, $00, $00, $00, $00, $16, $8E, $20, $00, $00, $00, $00, $00, $01, $00, $00, $00, $38, $00, $00, $07, $A2, $00, $10, $00,
      $00, $00, $00, $00, $56, $0D, $10, $00, $00, $00, $00, $00, $56, $0D, $10, $00, $01, $00, $00, $00, $32, $00, $00, $0A, $32, $00, $10, $00, $00, $00, $00, $00, $86, $00, $10, $00, $00, $00, $00, $00,
      $86, $00, $10, $00, $01, $00, $00, $00, $D6, $05, $10, $80, $41, $00, $00, $00, $00, $00, $00, $00, $38, $00, $00, $07, $42, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00, $01, $00, $00, $00,
      $2A, $00, $10, $00, $01, $00, $00, $00, $32, $00, $00, $0A, $42, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00, $01, $00, $00, $00, $3A, $00, $10, $00, $01, $00, $00, $00, $2A, $00, $10, $80,
      $41, $00, $00, $00, $00, $00, $00, $00, $38, $00, $00, $07, $32, $00, $10, $00, $00, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $A6, $0A, $10, $00, $00, $00, $00, $00, $31, $00, $00, $0A,
      $32, $00, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $01, $00, $00, $07,
      $12, $00, $10, $00, $00, $00, $00, $00, $1A, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00, $00, $00, $00, $00, $1F, $00, $04, $03, $0A, $00, $10, $00, $00, $00, $00, $00, $36, $00, $00, $05,
      $F2, $20, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $03, $00, $00, $00, $12, $00, $00, $01, $00, $00, $00, $09, $F2, $00, $10, $00, $02, $00, $00, $00, $46, $11, $10, $80, $41, $00, $00, $00,
      $00, $00, $00, $00, $E6, $8B, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $38, $00, $00, $07, $32, $00, $10, $00, $00, $00, $00, $00, $D6, $05, $10, $00, $01, $00, $00, $00, $D6, $05, $10, $00,
      $02, $00, $00, $00, $32, $00, $00, $0A, $32, $00, $10, $00, $00, $00, $00, $00, $86, $00, $10, $00, $02, $00, $00, $00, $86, $00, $10, $00, $01, $00, $00, $00, $46, $00, $10, $80, $41, $00, $00, $00,
      $00, $00, $00, $00, $38, $00, $00, $07, $32, $00, $10, $00, $00, $00, $00, $00, $A6, $0A, $10, $00, $00, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $31, $00, $00, $0A, $32, $00, $10, $00,
      $00, $00, $00, $00, $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $01, $00, $00, $07, $12, $00, $10, $00,
      $00, $00, $00, $00, $1A, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00, $00, $00, $00, $00, $1F, $00, $04, $03, $0A, $00, $10, $00, $00, $00, $00, $00, $45, $00, $00, $09, $F2, $20, $10, $00,
      $00, $00, $00, $00, $46, $10, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00, $01, $00, $00, $00, $00, $60, $10, $00, $01, $00, $00, $00, $12, $00, $00, $01, $45, $00, $00, $09, $F2, $20, $10, $00,
      $00, $00, $00, $00, $46, $10, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00, $00, $00, $00, $00, $00, $60, $10, $00, $00, $00, $00, $00, $15, $00, $00, $01, $15, $00, $00, $01, $15, $00, $00, $01,
      $3E, $00, $00, $01, $53, $54, $41, $54, $74, $00, $00, $00, $89, $00, $00, $00, $07, $00, $00, $00, $00, $00, $00, $00, $02, $00, $00, $00, $46, $00, $00, $00, $00, $00, $00, $00, $0B, $00, $00, $00,
      $09, $00, $00, $00, $09, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $09, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $06, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Target', TContextShaderVariableKind.Texture, 1, 0),
      TContextShaderVariable.Create('Back', TContextShaderVariableKind.Texture, 2, 0),
      TContextShaderVariable.Create('APoint', TContextShaderVariableKind.Vector, 0, 8),
      TContextShaderVariable.Create('A1Point', TContextShaderVariableKind.Vector, 8, 8),
      TContextShaderVariable.Create('BPoint', TContextShaderVariableKind.Vector, 16, 8),
      TContextShaderVariable.Create('CPoint', TContextShaderVariableKind.Vector, 24, 8),
      TContextShaderVariable.Create('LineAB', TContextShaderVariableKind.Vector, 32, 8),
      TContextShaderVariable.Create('LineAB1', TContextShaderVariableKind.Vector, 40, 8),
      TContextShaderVariable.Create('K', TContextShaderVariableKind.Float, 48, 4),
      TContextShaderVariable.Create('Len', TContextShaderVariableKind.Float, 52, 4)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.Metal'}
    {$IF defined(MACOS)}
    TContextShaderSource.Create(
      TContextShaderArch.Metal,
      TEncoding.UTF8.GetBytes(
        'using namespace metal;'+

        'struct ProjectedVertex {'+
          'float4 position [[position]];'+
          'float2 textureCoord;'+
        '};'+

        'float Cross(float2 PointA, float2 PointB) {'+
          'return PointA.x * PointB.y - PointB.x*PointA.y;'+
        '}'+

        'bool PointInTriangle(float2 TestPoint, float2 PointA, float2 BPoint, float2 CPoint) {'+
          'return ((Cross(PointA - TestPoint,BPoint - TestPoint)*Cross(BPoint - TestPoint,CPoint - TestPoint))>0)'+
            '&& ((Cross(BPoint - TestPoint,CPoint - TestPoint)*Cross(CPoint - TestPoint,PointA - TestPoint))>0);'+
        '}'+

        'float4 GetMainTexture(float2 SomePoint, const texture2d<float> Input, const sampler InputSampler) {'+
          'return Input.sample(InputSampler, SomePoint);'+
        '}'+

        'float4 GetFon(float2 SomePoint, const texture2d<float> Target, const sampler TargetSampler) {'+
          'return Target.sample(TargetSampler, SomePoint);'+
        '}'+

        'float2 norm(float2 val, float  K) {'+
          'val.x = val.x * K ;'+
          'return val;'+
        '}'+

        'float2 unnorm(float2 val, float  K) {'+
          'val.x = val.x / K ;'+
          'return val;'+
        '}'+

        'float4 GetSubstrate(float2 SomePoint,'+
                            'float2 APoint,'+
                            'float2 A1Point,'+
                            'float  K,'+
                            'const texture2d<float> Back,'+
                            'const sampler BackSampler) {'+
          'float2 _APoint = norm(APoint, K);'+
          'float2 _A1Point = norm(A1Point, K);'+
          'SomePoint = norm(SomePoint, K);'+
          'float2 NVec = normalize(_APoint - _A1Point) ;'+
          'SomePoint = reflect(SomePoint, NVec);'+
          'float2 N1C = reflect(_A1Point, NVec);'+
          'float2 C = N1C;'+
          'SomePoint -= C - _APoint;'+
          'return Back.sample(BackSampler, unnorm(SomePoint, K));'+
        '}'+

        'float4 GetTexture(float2 TestPoint,'+
                          'float2 TruePoint,'+
                          'float2 APoint,'+
                          'float2 A1Point,'+
                          'float2 BPoint,'+
                          'float2 CPoint,'+
                          'float2 LineAB,'+
                          'float2 LineAB1,'+
                          'float  K,'+
                          'float  Len,'+
                          'const texture2d<float> Input,'+
                          'const sampler InputSampler,'+
                          'const texture2d<float> Target,'+
                          'const sampler TargetSampler,'+
                          'const texture2d<float> Back,'+
                          'const sampler BackSampler) {'+
          'if (PointInTriangle(TestPoint, APoint, BPoint, CPoint)) {'+
            'return GetSubstrate(TestPoint,APoint,A1Point,K,Back,BackSampler);'+
          '}'+
          'else{'+
            'if (PointInTriangle(TestPoint, A1Point, BPoint, CPoint)) {'+
              'return GetFon(TruePoint, Target, TargetSampler);'+
            '}'+
            'else{'+
              'return GetMainTexture(TestPoint, Input, InputSampler);'+
            '}'+
          '}'+
        '}'+

        'fragment float4 fragmentShader(const ProjectedVertex in [[stage_in]],'+
                                       'constant float4 &APoint [[buffer(0)]],'+
                                       'constant float4 &A1Point [[buffer(1)]],'+
                                       'constant float4 &BPoint [[buffer(2)]],'+
                                       'constant float4 &CPoint [[buffer(3)]],'+
                                       'constant float4 &LineAB [[buffer(4)]],'+
                                       'constant float4 &LineAB1 [[buffer(5)]],'+
                                       'constant float4 &K [[buffer(6)]],'+
                                       'constant float4 &Len [[buffer(7)]],'+
                                       'const texture2d<float> Input [[texture(0)]],'+
                                       'const sampler InputSampler [[sampler(0)]],'+
                                       'const texture2d<float> Target [[texture(1)]],'+
                                       'const sampler TargetSampler [[sampler(1)]],'+
                                       'const texture2d<float> Back [[texture(2)]],'+
                                       'const sampler BackSampler [[sampler(2)]]) {'+
          'return GetTexture(in.textureCoord,'+
                            'in.textureCoord,'+
                            'APoint.xy,'+
                            'A1Point.xy,'+
                            'BPoint.xy,'+
                            'CPoint.xy,'+
                            'LineAB.xy,'+
                            'LineAB1.xy,'+
                            'K.x,'+
                            'Len.x,'+
                            'Input,'+
                            'InputSampler,'+
                            'Target,'+
                            'TargetSampler,'+
                            'Back,'+
                            'BackSampler);'+
        '}'
      ),
      [TContextShaderVariable.Create('APoint', TContextShaderVariableKind.Float2, 0, 1),
       TContextShaderVariable.Create('A1Point', TContextShaderVariableKind.Float2, 1, 1),
       TContextShaderVariable.Create('BPoint', TContextShaderVariableKind.Float2, 2, 1),
       TContextShaderVariable.Create('CPoint', TContextShaderVariableKind.Float2, 3, 1),
       TContextShaderVariable.Create('LineAB', TContextShaderVariableKind.Float2, 4, 1),
       TContextShaderVariable.Create('LineAB1', TContextShaderVariableKind.Float2, 5, 1),
       TContextShaderVariable.Create('K', TContextShaderVariableKind.Float, 6, 1),
       TContextShaderVariable.Create('Len', TContextShaderVariableKind.Float, 7, 1),
       TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
       TContextShaderVariable.Create('Target', TContextShaderVariableKind.Texture, 1, 0),
       TContextShaderVariable.Create('Back', TContextShaderVariableKind.Texture, 2, 0)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.GLSL'}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $66, $6C, $6F, $61, $74, $20, $5F, $54, $4D, $50, $39, $3B, $0D, $0A, $66, $6C, $6F, $61, $74, $20,
      $5F, $54, $4D, $50, $38, $3B, $0D, $0A, $66, $6C, $6F, $61, $74, $20, $5F, $54, $4D, $50, $37, $3B, $0D, $0A, $66, $6C, $6F, $61, $74, $20, $5F, $54, $4D, $50, $36, $3B, $0D, $0A, $66, $6C, $6F, $61,
      $74, $20, $5F, $54, $4D, $50, $31, $32, $3B, $0D, $0A, $66, $6C, $6F, $61, $74, $20, $5F, $54, $4D, $50, $35, $3B, $0D, $0A, $66, $6C, $6F, $61, $74, $20, $5F, $54, $4D, $50, $34, $3B, $0D, $0A, $66,
      $6C, $6F, $61, $74, $20, $5F, $54, $4D, $50, $30, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $54, $4D, $50, $32, $3B, $0D, $0A, $66, $6C, $6F, $61, $74, $20, $5F, $54, $4D, $50, $33, $3B, $0D, $0A,
      $66, $6C, $6F, $61, $74, $20, $5F, $54, $4D, $50, $31, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $43, $63, $30, $30, $32, $37, $3B, $0D, $0A, $66, $6C, $6F, $61, $74, $20, $5F, $4B, $78, $30,
      $30, $32, $37, $3B, $0D, $0A, $66, $6C, $6F, $61, $74, $20, $5F, $4C, $4C, $65, $6E, $30, $30, $32, $37, $3B, $0D, $0A, $66, $6C, $6F, $61, $74, $20, $5F, $4B, $30, $30, $32, $37, $3B, $0D, $0A, $76,
      $65, $63, $32, $20, $5F, $43, $6F, $72, $72, $65, $63, $74, $30, $30, $32, $37, $3B, $0D, $0A, $66, $6C, $6F, $61, $74, $20, $5F, $54, $4D, $50, $32, $38, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F,
      $76, $30, $30, $32, $39, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $76, $30, $30, $33, $37, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $34, $32, $3B, $0D, $0A, $76, $65, $63, $34,
      $20, $5F, $54, $4D, $50, $34, $36, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $34, $38, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30,
      $30, $34, $39, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $4E, $56, $65, $63, $30, $30, $34, $39, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $4E, $31, $43, $30, $30, $34, $39, $3B, $0D, $0A, $76,
      $65, $63, $32, $20, $5F, $76, $61, $6C, $30, $30, $35, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $76, $61, $6C, $30, $30, $35, $33, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $76, $61, $6C,
      $30, $30, $35, $35, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $76, $30, $30, $35, $37, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $76, $61, $6C, $30, $30, $37, $31, $3B, $0D, $0A, $76, $65, $63,
      $34, $20, $5F, $54, $4D, $50, $37, $34, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $37, $38, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $38,
      $35, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $38, $35, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $38, $37, $3B,
      $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $38, $37, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $38, $39, $3B, $0D, $0A,
      $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $38, $39, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $39, $31, $3B, $0D, $0A, $76, $65,
      $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $39, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $39, $35, $3B, $0D, $0A, $76, $65, $63, $32,
      $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $39, $35, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $39, $37, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F,
      $50, $6F, $69, $6E, $74, $42, $30, $30, $39, $37, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $39, $39, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F,
      $69, $6E, $74, $42, $30, $30, $39, $39, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $30, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E,
      $74, $42, $30, $31, $30, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $31, $30, $32, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $31, $30, $34, $3B, $0D, $0A, $76,
      $65, $63, $32, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30, $31, $30, $35, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $4E, $56, $65, $63, $30, $31, $30, $35, $3B, $0D, $0A, $76, $65,
      $63, $32, $20, $5F, $4E, $31, $43, $30, $31, $30, $35, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $76, $61, $6C, $30, $31, $30, $37, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $76, $61, $6C, $30,
      $31, $30, $39, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $76, $61, $6C, $30, $31, $31, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $76, $30, $31, $31, $33, $3B, $0D, $0A, $76, $65, $63, $32,
      $20, $5F, $76, $61, $6C, $30, $31, $32, $37, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $31, $33, $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $31, $33, $34, $3B,
      $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $34, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $34, $31, $3B, $0D, $0A,
      $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $34, $33, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $34, $33, $3B, $0D, $0A, $76, $65,
      $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $34, $35, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $34, $35, $3B, $0D, $0A, $76, $65, $63, $32,
      $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $34, $37, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $34, $37, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F,
      $50, $6F, $69, $6E, $74, $41, $30, $31, $35, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $35, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F,
      $69, $6E, $74, $41, $30, $31, $35, $33, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $35, $33, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E,
      $74, $41, $30, $31, $35, $35, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $35, $35, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $41,
      $30, $31, $35, $37, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $35, $37, $3B, $0D, $0A, $66, $6C, $6F, $61, $74, $20, $5F, $61, $30, $31, $36, $31, $3B, $0D,
      $0A, $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $31, $36, $32, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $31, $36, $34, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $53, $6F, $6D,
      $65, $50, $6F, $69, $6E, $74, $30, $31, $36, $35, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $4E, $56, $65, $63, $30, $31, $36, $35, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $4E, $31, $43, $30,
      $31, $36, $35, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $76, $61, $6C, $30, $31, $36, $37, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $76, $61, $6C, $30, $31, $36, $39, $3B, $0D, $0A, $76, $65,
      $63, $32, $20, $5F, $76, $61, $6C, $30, $31, $37, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $76, $30, $31, $37, $33, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $76, $61, $6C, $30, $31, $38,
      $37, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $31, $39, $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $54, $4D, $50, $31, $39, $34, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F,
      $50, $6F, $69, $6E, $74, $41, $30, $32, $30, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $30, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F,
      $69, $6E, $74, $41, $30, $32, $30, $33, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $30, $33, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E,
      $74, $41, $30, $32, $30, $35, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $30, $35, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $41,
      $30, $32, $30, $37, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $30, $37, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $32,
      $31, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $31, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $31, $33,
      $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $31, $33, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $31, $35, $3B, $0D,
      $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $31, $35, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $31, $37, $3B, $0D, $0A, $76,
      $65, $63, $32, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $31, $37, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $32, $20, $5F, $41, $50, $6F, $69, $6E, $74, $3B, $0D,
      $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $32, $20, $5F, $41, $31, $50, $6F, $69, $6E, $74, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $32, $20, $5F, $42,
      $50, $6F, $69, $6E, $74, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $32, $20, $5F, $43, $50, $6F, $69, $6E, $74, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76,
      $65, $63, $32, $20, $5F, $4C, $69, $6E, $65, $41, $42, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $32, $20, $5F, $4C, $69, $6E, $65, $41, $42, $31, $3B, $0D, $0A, $75, $6E,
      $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $5F, $4B, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $5F, $4C, $65, $6E, $3B, $0D, $0A, $75, $6E,
      $69, $66, $6F, $72, $6D, $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $5F, $49, $6E, $70, $75, $74, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $61, $6D, $70, $6C, $65, $72,
      $32, $44, $20, $5F, $54, $61, $72, $67, $65, $74, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $5F, $42, $61, $63, $6B, $3B, $0D, $0A, $76,
      $6F, $69, $64, $20, $6D, $61, $69, $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20, $20, $76, $65, $63, $34, $20, $5F, $52, $65, $74, $56, $61, $6C, $3B, $0D, $0A, $20, $20, $20, $20, $76, $65,
      $63, $32, $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $3B, $0D, $0A, $20, $20, $20, $20, $69, $66, $20, $28, $5F, $4C, $69, $6E, $65, $41, $42, $2E, $78, $2A, $54, $45, $58,
      $30, $2E, $78, $20, $2B, $20, $5F, $4C, $69, $6E, $65, $41, $42, $2E, $79, $20, $3C, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $26, $26, $20, $5F, $4C, $69, $6E, $65, $41, $42, $31, $2E, $78, $2A,
      $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $5F, $4C, $69, $6E, $65, $41, $42, $31, $2E, $79, $20, $3E, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $7C, $7C, $20, $5F, $4C, $69, $6E, $65, $41, $42,
      $2E, $78, $2A, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $5F, $4C, $69, $6E, $65, $41, $42, $2E, $79, $20, $3E, $3D, $20, $54, $45, $58, $30, $2E, $79, $20, $26, $26, $20, $5F, $4C, $69, $6E, $65,
      $41, $42, $31, $2E, $78, $2A, $54, $45, $58, $30, $2E, $78, $20, $2B, $20, $5F, $4C, $69, $6E, $65, $41, $42, $31, $2E, $79, $20, $3C, $3D, $20, $54, $45, $58, $30, $2E, $79, $29, $20, $7B, $20, $0D,
      $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $43, $63, $30, $30, $32, $37, $20, $3D, $20, $76, $65, $63, $32, $28, $28, $54, $45, $58, $30, $2E, $79, $20, $2D, $20, $5F, $4C, $69, $6E, $65, $41,
      $42, $31, $2E, $79, $29, $2F, $5F, $4C, $69, $6E, $65, $41, $42, $31, $2E, $78, $2C, $20, $54, $45, $58, $30, $2E, $79, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $30, $30,
      $32, $39, $20, $3D, $20, $5F, $41, $50, $6F, $69, $6E, $74, $20, $2D, $20, $5F, $41, $31, $50, $6F, $69, $6E, $74, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $33, $20,
      $3D, $20, $64, $6F, $74, $28, $5F, $76, $30, $30, $32, $39, $2C, $20, $5F, $76, $30, $30, $32, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $31, $20, $3D,
      $20, $69, $6E, $76, $65, $72, $73, $65, $73, $71, $72, $74, $28, $5F, $54, $4D, $50, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $38, $20, $3D, $20, $31,
      $2E, $30, $2F, $5F, $54, $4D, $50, $31, $31, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $4B, $78, $30, $30, $32, $37, $20, $3D, $20, $5F, $54, $4D, $50, $32, $38, $2F, $28, $5F, $43,
      $50, $6F, $69, $6E, $74, $20, $2D, $20, $5F, $41, $31, $50, $6F, $69, $6E, $74, $29, $2E, $78, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $4C, $4C, $65, $6E, $30, $30, $32, $37, $20,
      $3D, $20, $5F, $4B, $78, $30, $30, $32, $37, $2A, $28, $5F, $43, $63, $30, $30, $32, $37, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $29, $2E, $78, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20,
      $20, $20, $5F, $4B, $30, $30, $32, $37, $20, $3D, $20, $28, $28, $28, $28, $28, $28, $28, $5F, $4C, $4C, $65, $6E, $30, $30, $32, $37, $2F, $5F, $4C, $65, $6E, $29, $2A, $5F, $4C, $4C, $65, $6E, $30,
      $30, $32, $37, $29, $2F, $5F, $4C, $65, $6E, $29, $2A, $5F, $4C, $4C, $65, $6E, $30, $30, $32, $37, $29, $2F, $5F, $4C, $65, $6E, $29, $2A, $5F, $4C, $4C, $65, $6E, $30, $30, $32, $37, $29, $2F, $5F,
      $4C, $65, $6E, $29, $2A, $5F, $4C, $65, $6E, $2A, $31, $2E, $30, $30, $30, $30, $30, $30, $30, $31, $45, $2D, $30, $30, $31, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $30, $30,
      $33, $37, $20, $3D, $20, $5F, $41, $31, $50, $6F, $69, $6E, $74, $20, $2D, $20, $5F, $41, $50, $6F, $69, $6E, $74, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $34, $20,
      $3D, $20, $64, $6F, $74, $28, $5F, $76, $30, $30, $33, $37, $2C, $20, $5F, $76, $30, $30, $33, $37, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $35, $20, $3D, $20,
      $69, $6E, $76, $65, $72, $73, $65, $73, $71, $72, $74, $28, $5F, $54, $4D, $50, $34, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $32, $20, $3D, $20, $5F, $54, $4D,
      $50, $35, $2A, $5F, $76, $30, $30, $33, $37, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $43, $6F, $72, $72, $65, $63, $74, $30, $30, $32, $37, $20, $3D, $20, $5F, $4B, $30, $30, $32,
      $37, $2A, $5F, $54, $4D, $50, $32, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $20, $3D, $20, $54, $45, $58, $30, $2E, $78,
      $79, $20, $2B, $20, $5F, $43, $6F, $72, $72, $65, $63, $74, $30, $30, $32, $37, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $69, $66, $20, $28, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F,
      $69, $6E, $74, $31, $2E, $78, $20, $3C, $20, $30, $2E, $30, $20, $7C, $7C, $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $2E, $79, $20, $3C, $20, $30, $2E, $30, $20, $7C, $7C,
      $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $2E, $78, $20, $3E, $20, $31, $2E, $30, $20, $7C, $7C, $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $2E,
      $79, $20, $3E, $20, $31, $2E, $30, $29, $20, $7B, $20, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $34, $32, $20, $3D, $20, $74, $65, $78, $74, $75, $72,
      $65, $32, $44, $28, $5F, $54, $61, $72, $67, $65, $74, $2C, $20, $54, $45, $58, $30, $2E, $78, $79, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $52, $65, $74,
      $56, $61, $6C, $20, $3D, $20, $5F, $54, $4D, $50, $34, $32, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $7D, $20, $65, $6C, $73, $65, $20, $7B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $39, $35, $20, $3D, $20, $5F, $41, $50, $6F, $69, $6E, $74, $20, $2D, $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E,
      $74, $31, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $39, $35, $20, $3D, $20, $5F, $42, $50, $6F, $69, $6E, $74, $20, $2D,
      $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $5F, $50, $6F,
      $69, $6E, $74, $41, $30, $30, $39, $35, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $39, $35, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $39, $35, $2E, $78,
      $2A, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $39, $35, $2E, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $39, $37,
      $20, $3D, $20, $5F, $42, $50, $6F, $69, $6E, $74, $20, $2D, $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $39, $37, $20, $3D, $20, $5F, $43, $50, $6F, $69, $6E, $74, $20, $2D, $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $3B, $0D,
      $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $39, $37, $2E, $78, $2A, $5F, $50, $6F, $69, $6E,
      $74, $42, $30, $30, $39, $37, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $39, $37, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $39, $37, $2E, $79, $3B, $0D,
      $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $39, $39, $20, $3D, $20, $5F, $42, $50, $6F, $69, $6E, $74, $20, $2D, $20, $5F, $43, $61,
      $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $39, $39, $20, $3D, $20, $5F,
      $43, $50, $6F, $69, $6E, $74, $20, $2D, $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D,
      $50, $38, $20, $3D, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $39, $39, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $39, $39, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E,
      $74, $42, $30, $30, $39, $39, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $39, $39, $2E, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F,
      $69, $6E, $74, $41, $30, $31, $30, $31, $20, $3D, $20, $5F, $43, $50, $6F, $69, $6E, $74, $20, $2D, $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $3B, $0D, $0A, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $30, $31, $20, $3D, $20, $5F, $41, $50, $6F, $69, $6E, $74, $20, $2D, $20, $5F, $43, $61, $6C, $63, $65, $64,
      $50, $6F, $69, $6E, $74, $31, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $30, $31,
      $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $30, $31, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $30, $31, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $41,
      $30, $31, $30, $31, $2E, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $69, $66, $20, $28, $5F, $54, $4D, $50, $36, $2A, $5F, $54, $4D, $50, $37, $20, $3E, $20, $30,
      $2E, $30, $20, $26, $26, $20, $5F, $54, $4D, $50, $38, $2A, $5F, $54, $4D, $50, $39, $20, $3E, $20, $30, $2E, $30, $29, $20, $7B, $20, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $30, $35, $31, $20, $3D, $20, $5F, $41, $50, $6F, $69, $6E, $74, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $5F, $76, $61, $6C, $30, $30, $35, $31, $2E, $78, $20, $3D, $20, $5F, $41, $50, $6F, $69, $6E, $74, $2E, $78, $2A, $5F, $4B, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $30, $35, $33, $20, $3D, $20, $5F, $41, $31, $50, $6F, $69, $6E, $74, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $30, $35, $33, $2E, $78, $20, $3D, $20, $5F, $41, $31, $50, $6F, $69, $6E, $74, $2E, $78, $2A, $5F, $4B, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $30, $35, $35, $20, $3D, $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $3B, $0D, $0A, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $30, $35, $35, $2E, $78, $20, $3D, $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $2E, $78,
      $2A, $5F, $4B, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $30, $30, $35, $37, $20, $3D, $20, $5F, $76, $61, $6C, $30, $30, $35, $31, $20,
      $2D, $20, $5F, $76, $61, $6C, $30, $30, $35, $33, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $34, $20, $3D, $20, $64, $6F, $74,
      $28, $5F, $76, $30, $30, $35, $37, $2C, $20, $5F, $76, $30, $30, $35, $37, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $35,
      $20, $3D, $20, $69, $6E, $76, $65, $72, $73, $65, $73, $71, $72, $74, $28, $5F, $54, $4D, $50, $34, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $5F, $4E, $56, $65, $63, $30, $30, $34, $39, $20, $3D, $20, $5F, $54, $4D, $50, $35, $2A, $5F, $76, $30, $30, $35, $37, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $5F, $54, $4D, $50, $31, $32, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4E, $56, $65, $63, $30, $30, $34, $39, $2C, $20, $5F, $76, $61, $6C, $30, $30, $35, $35, $29, $3B, $0D, $0A, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30, $30, $34, $39, $20, $3D, $20, $5F, $76, $61, $6C, $30, $30, $35, $35,
      $20, $2D, $20, $28, $32, $2E, $30, $2A, $5F, $4E, $56, $65, $63, $30, $30, $34, $39, $29, $2A, $5F, $54, $4D, $50, $31, $32, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $32, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4E, $56, $65, $63, $30, $30, $34, $39, $2C, $20, $5F, $76, $61, $6C, $30, $30, $35, $33, $29, $3B, $0D, $0A,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $4E, $31, $43, $30, $30, $34, $39, $20, $3D, $20, $5F, $76, $61, $6C, $30, $30, $35, $33, $20, $2D, $20, $28, $32,
      $2E, $30, $2A, $5F, $4E, $56, $65, $63, $30, $30, $34, $39, $29, $2A, $5F, $54, $4D, $50, $31, $32, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F,
      $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30, $30, $34, $39, $20, $3D, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30, $30, $34, $39, $20, $2D, $20, $28, $5F, $4E, $31, $43, $30, $30,
      $34, $39, $20, $2D, $20, $5F, $76, $61, $6C, $30, $30, $35, $31, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $53, $6F, $6D, $65, $50, $6F,
      $69, $6E, $74, $30, $30, $34, $39, $2E, $79, $20, $3D, $20, $31, $2E, $30, $20, $2D, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30, $30, $34, $39, $2E, $79, $3B, $0D, $0A, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $30, $37, $31, $20, $3D, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30, $30, $34, $39, $3B, $0D,
      $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $30, $37, $31, $2E, $78, $20, $3D, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74,
      $30, $30, $34, $39, $2E, $78, $2F, $5F, $4B, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $34, $38, $20, $3D, $20, $74, $65, $78,
      $74, $75, $72, $65, $32, $44, $28, $5F, $42, $61, $63, $6B, $2C, $20, $5F, $76, $61, $6C, $30, $30, $37, $31, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $5F, $54, $4D, $50, $34, $36, $20, $3D, $20, $5F, $54, $4D, $50, $34, $38, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $7D, $20, $65, $6C, $73, $65, $20, $7B,
      $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $38, $35, $20, $3D, $20, $5F, $41, $31, $50, $6F, $69, $6E, $74,
      $20, $2D, $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E,
      $74, $42, $30, $30, $38, $35, $20, $3D, $20, $5F, $42, $50, $6F, $69, $6E, $74, $20, $2D, $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $3B, $0D, $0A, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $38, $35, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42,
      $30, $30, $38, $35, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $38, $35, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $38, $35, $2E, $79, $3B, $0D, $0A, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $38, $37, $20, $3D, $20, $5F, $42, $50, $6F, $69, $6E, $74, $20, $2D, $20, $5F,
      $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30,
      $38, $37, $20, $3D, $20, $5F, $43, $50, $6F, $69, $6E, $74, $20, $2D, $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $38, $37, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $38, $37,
      $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $38, $37, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $38, $37, $2E, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $38, $39, $20, $3D, $20, $5F, $42, $50, $6F, $69, $6E, $74, $20, $2D, $20, $5F, $43, $61, $6C, $63,
      $65, $64, $50, $6F, $69, $6E, $74, $31, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $38, $39, $20, $3D,
      $20, $5F, $43, $50, $6F, $69, $6E, $74, $20, $2D, $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $5F, $54, $4D, $50, $38, $20, $3D, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $38, $39, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $38, $39, $2E, $79, $20, $2D,
      $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $38, $39, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $38, $39, $2E, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $39, $31, $20, $3D, $20, $5F, $43, $50, $6F, $69, $6E, $74, $20, $2D, $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F,
      $69, $6E, $74, $31, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $39, $31, $20, $3D, $20, $5F, $41, $31,
      $50, $6F, $69, $6E, $74, $20, $2D, $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $5F, $54, $4D, $50, $39, $20, $3D, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $39, $31, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $30, $39, $31, $2E, $79, $20, $2D, $20, $5F, $50,
      $6F, $69, $6E, $74, $42, $30, $30, $39, $31, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $41, $30, $30, $39, $31, $2E, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $69, $66, $20, $28, $5F, $54, $4D, $50, $36, $2A, $5F, $54, $4D, $50, $37, $20, $3E, $20, $30, $2E, $30, $20, $26, $26, $20, $5F, $54, $4D, $50, $38, $2A, $5F, $54, $4D, $50, $39,
      $20, $3E, $20, $30, $2E, $30, $29, $20, $7B, $20, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $34, $20, $3D,
      $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $54, $61, $72, $67, $65, $74, $2C, $20, $54, $45, $58, $30, $2E, $78, $79, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $34, $36, $20, $3D, $20, $5F, $54, $4D, $50, $37, $34, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $7D, $20, $65, $6C, $73, $65, $20, $7B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $37,
      $38, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $43, $61, $6C, $63, $65, $64, $50, $6F, $69, $6E, $74, $31, $29, $3B, $0D, $0A, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $34, $36, $20, $3D, $20, $5F, $54, $4D, $50, $37, $38, $3B, $0D, $0A, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $7D, $20, $2F, $2F, $20, $65, $6E, $64, $20, $69, $66, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $7D, $20,
      $2F, $2F, $20, $65, $6E, $64, $20, $69, $66, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $52, $65, $74, $56, $61, $6C, $20, $3D, $20, $5F, $54, $4D, $50, $34, $36, $3B,
      $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $7D, $20, $2F, $2F, $20, $65, $6E, $64, $20, $69, $66, $0D, $0A, $20, $20, $20, $20, $7D, $20, $65, $6C, $73, $65, $20, $7B, $0D, $0A, $20, $20, $20,
      $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $35, $31, $20, $3D, $20, $5F, $41, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20,
      $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $35, $31, $20, $3D, $20, $5F, $42, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D,
      $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $35, $31, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31,
      $35, $31, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $35, $31, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $35, $31, $2E, $79, $3B, $0D, $0A, $20, $20, $20,
      $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $35, $33, $20, $3D, $20, $5F, $42, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20,
      $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $35, $33, $20, $3D, $20, $5F, $43, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D,
      $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $35, $33, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31,
      $35, $33, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $35, $33, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $35, $33, $2E, $79, $3B, $0D, $0A, $20, $20, $20,
      $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $35, $35, $20, $3D, $20, $5F, $42, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20,
      $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $35, $35, $20, $3D, $20, $5F, $43, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D,
      $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $38, $20, $3D, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $35, $35, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31,
      $35, $35, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $35, $35, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $35, $35, $2E, $79, $3B, $0D, $0A, $20, $20, $20,
      $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $35, $37, $20, $3D, $20, $5F, $43, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20,
      $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $35, $37, $20, $3D, $20, $5F, $41, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D,
      $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $35, $37, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31,
      $35, $37, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $35, $37, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $35, $37, $2E, $79, $3B, $0D, $0A, $20, $20, $20,
      $20, $20, $20, $20, $20, $69, $66, $20, $28, $5F, $54, $4D, $50, $36, $2A, $5F, $54, $4D, $50, $37, $20, $3E, $20, $30, $2E, $30, $20, $26, $26, $20, $5F, $54, $4D, $50, $38, $2A, $5F, $54, $4D, $50,
      $39, $20, $3E, $20, $30, $2E, $30, $29, $20, $7B, $20, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $31, $30, $37, $20, $3D, $20, $5F, $41, $50, $6F,
      $69, $6E, $74, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $31, $30, $37, $2E, $78, $20, $3D, $20, $5F, $41, $50, $6F, $69, $6E, $74, $2E, $78,
      $2A, $5F, $4B, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $31, $30, $39, $20, $3D, $20, $5F, $41, $31, $50, $6F, $69, $6E, $74, $3B, $0D, $0A,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $31, $30, $39, $2E, $78, $20, $3D, $20, $5F, $41, $31, $50, $6F, $69, $6E, $74, $2E, $78, $2A, $5F, $4B, $3B, $0D,
      $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $31, $31, $31, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $31, $31, $31, $2E, $78, $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $2A, $5F, $4B, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $5F, $76, $30, $31, $31, $33, $20, $3D, $20, $5F, $76, $61, $6C, $30, $31, $30, $37, $20, $2D, $20, $5F, $76, $61, $6C, $30, $31, $30, $39, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $34, $20, $3D, $20, $64, $6F, $74, $28, $5F, $76, $30, $31, $31, $33, $2C, $20, $5F, $76, $30, $31, $31, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $35, $20, $3D, $20, $69, $6E, $76, $65, $72, $73, $65, $73, $71, $72, $74, $28, $5F, $54, $4D, $50, $34, $29, $3B, $0D, $0A, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $4E, $56, $65, $63, $30, $31, $30, $35, $20, $3D, $20, $5F, $54, $4D, $50, $35, $2A, $5F, $76, $30, $31, $31, $33, $3B, $0D, $0A, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $32, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4E, $56, $65, $63, $30, $31, $30, $35, $2C, $20, $5F, $76, $61, $6C, $30, $31, $31, $31,
      $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30, $31, $30, $35, $20, $3D, $20, $5F, $76, $61, $6C, $30, $31, $31,
      $31, $20, $2D, $20, $28, $32, $2E, $30, $2A, $5F, $4E, $56, $65, $63, $30, $31, $30, $35, $29, $2A, $5F, $54, $4D, $50, $31, $32, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $5F, $54, $4D, $50, $31, $32, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4E, $56, $65, $63, $30, $31, $30, $35, $2C, $20, $5F, $76, $61, $6C, $30, $31, $30, $39, $29, $3B, $0D, $0A, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $4E, $31, $43, $30, $31, $30, $35, $20, $3D, $20, $5F, $76, $61, $6C, $30, $31, $30, $39, $20, $2D, $20, $28, $32, $2E, $30, $2A, $5F, $4E, $56, $65,
      $63, $30, $31, $30, $35, $29, $2A, $5F, $54, $4D, $50, $31, $32, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30, $31,
      $30, $35, $20, $3D, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30, $31, $30, $35, $20, $2D, $20, $28, $5F, $4E, $31, $43, $30, $31, $30, $35, $20, $2D, $20, $5F, $76, $61, $6C, $30, $31,
      $30, $37, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30, $31, $30, $35, $2E, $79, $20, $3D, $20, $31, $2E, $30,
      $20, $2D, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30, $31, $30, $35, $2E, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $31,
      $32, $37, $20, $3D, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30, $31, $30, $35, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $31,
      $32, $37, $2E, $78, $20, $3D, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30, $31, $30, $35, $2E, $78, $2F, $5F, $4B, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $5F, $54, $4D, $50, $31, $30, $34, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $42, $61, $63, $6B, $2C, $20, $5F, $76, $61, $6C, $30, $31, $32, $37, $29, $3B, $0D, $0A,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $30, $32, $20, $3D, $20, $5F, $54, $4D, $50, $31, $30, $34, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20,
      $7D, $20, $65, $6C, $73, $65, $20, $7B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $34, $31, $20, $3D, $20, $5F, $41, $31, $50,
      $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $34, $31,
      $20, $3D, $20, $5F, $42, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $36,
      $20, $3D, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $34, $31, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $34, $31, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42,
      $30, $31, $34, $31, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $34, $31, $2E, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E,
      $74, $41, $30, $31, $34, $33, $20, $3D, $20, $5F, $42, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $34, $33, $20, $3D, $20, $5F, $43, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $34, $33, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $34, $33,
      $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $34, $33, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $34, $33, $2E, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $34, $35, $20, $3D, $20, $5F, $42, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D,
      $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $34, $35, $20, $3D, $20, $5F, $43, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58,
      $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $38, $20, $3D, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $34, $35, $2E, $78,
      $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $34, $35, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $34, $35, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31,
      $34, $35, $2E, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $34, $37, $20, $3D, $20, $5F, $43, $50, $6F, $69, $6E, $74,
      $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $34, $37, $20, $3D, $20, $5F,
      $41, $31, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20,
      $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $34, $37, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $34, $37, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $31, $34,
      $37, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $41, $30, $31, $34, $37, $2E, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $69, $66, $20, $28, $5F, $54, $4D, $50,
      $36, $2A, $5F, $54, $4D, $50, $37, $20, $3E, $20, $30, $2E, $30, $20, $26, $26, $20, $5F, $54, $4D, $50, $38, $2A, $5F, $54, $4D, $50, $39, $20, $3E, $20, $30, $2E, $30, $29, $20, $7B, $20, $0D, $0A,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $33, $30, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $54, $61, $72,
      $67, $65, $74, $2C, $20, $54, $45, $58, $30, $2E, $78, $79, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $30, $32, $20,
      $3D, $20, $5F, $54, $4D, $50, $31, $33, $30, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $7D, $20, $65, $6C, $73, $65, $20, $7B, $0D, $0A, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $33, $34, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $54, $45,
      $58, $30, $2E, $78, $79, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $30, $32, $20, $3D, $20, $5F, $54, $4D, $50, $31,
      $33, $34, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $7D, $20, $2F, $2F, $20, $65, $6E, $64, $20, $69, $66, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $7D, $20,
      $2F, $2F, $20, $65, $6E, $64, $20, $69, $66, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $52, $65, $74, $56, $61, $6C, $20, $3D, $20, $5F, $54, $4D, $50, $31, $30, $32, $3B, $0D, $0A, $20,
      $20, $20, $20, $7D, $20, $2F, $2F, $20, $65, $6E, $64, $20, $69, $66, $0D, $0A, $20, $20, $20, $20, $5F, $61, $30, $31, $36, $31, $20, $3D, $20, $28, $54, $45, $58, $30, $2E, $78, $2A, $5F, $4C, $69,
      $6E, $65, $41, $42, $31, $2E, $78, $20, $2B, $20, $5F, $4C, $69, $6E, $65, $41, $42, $31, $2E, $79, $29, $20, $2D, $20, $54, $45, $58, $30, $2E, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D,
      $50, $30, $20, $3D, $20, $61, $62, $73, $28, $5F, $61, $30, $31, $36, $31, $29, $3B, $0D, $0A, $20, $20, $20, $20, $69, $66, $20, $28, $5F, $54, $4D, $50, $30, $20, $3C, $20, $31, $2E, $30, $30, $30,
      $30, $30, $30, $30, $35, $45, $2D, $30, $30, $33, $29, $20, $7B, $20, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $31, $31, $20, $3D, $20, $5F, $41,
      $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $31, $31, $20, $3D, $20,
      $5F, $42, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $5F, $50, $6F, $69,
      $6E, $74, $41, $30, $32, $31, $31, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $31, $31, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $31, $31, $2E, $78, $2A,
      $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $31, $31, $2E, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $31, $33, $20, $3D, $20, $5F, $42,
      $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $31, $33, $20, $3D, $20,
      $5F, $43, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $5F, $50, $6F, $69,
      $6E, $74, $41, $30, $32, $31, $33, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $31, $33, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $31, $33, $2E, $78, $2A,
      $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $31, $33, $2E, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $31, $35, $20, $3D, $20, $5F, $42,
      $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $31, $35, $20, $3D, $20,
      $5F, $43, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $38, $20, $3D, $20, $5F, $50, $6F, $69,
      $6E, $74, $41, $30, $32, $31, $35, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $31, $35, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $31, $35, $2E, $78, $2A,
      $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $31, $35, $2E, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $31, $37, $20, $3D, $20, $5F, $43,
      $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $31, $37, $20, $3D, $20,
      $5F, $41, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $5F, $50, $6F, $69,
      $6E, $74, $41, $30, $32, $31, $37, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $31, $37, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $31, $37, $2E, $78, $2A,
      $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $31, $37, $2E, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $69, $66, $20, $28, $5F, $54, $4D, $50, $36, $2A, $5F, $54, $4D, $50, $37, $20,
      $3E, $20, $30, $2E, $30, $20, $26, $26, $20, $5F, $54, $4D, $50, $38, $2A, $5F, $54, $4D, $50, $39, $20, $3E, $20, $30, $2E, $30, $29, $20, $7B, $20, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $31, $36, $37, $20, $3D, $20, $5F, $41, $50, $6F, $69, $6E, $74, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $61,
      $6C, $30, $31, $36, $37, $2E, $78, $20, $3D, $20, $5F, $41, $50, $6F, $69, $6E, $74, $2E, $78, $2A, $5F, $4B, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $61,
      $6C, $30, $31, $36, $39, $20, $3D, $20, $5F, $41, $31, $50, $6F, $69, $6E, $74, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $31, $36, $39, $2E,
      $78, $20, $3D, $20, $5F, $41, $31, $50, $6F, $69, $6E, $74, $2E, $78, $2A, $5F, $4B, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $31, $37, $31,
      $20, $3D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $31, $37, $31, $2E, $78, $20, $3D, $20, $54, $45,
      $58, $30, $2E, $78, $2A, $5F, $4B, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $30, $31, $37, $33, $20, $3D, $20, $5F, $76, $61, $6C, $30, $31, $36, $37, $20,
      $2D, $20, $5F, $76, $61, $6C, $30, $31, $36, $39, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $34, $20, $3D, $20, $64, $6F, $74, $28, $5F, $76, $30,
      $31, $37, $33, $2C, $20, $5F, $76, $30, $31, $37, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $35, $20, $3D, $20, $69, $6E, $76, $65, $72,
      $73, $65, $73, $71, $72, $74, $28, $5F, $54, $4D, $50, $34, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $4E, $56, $65, $63, $30, $31, $36, $35, $20, $3D, $20,
      $5F, $54, $4D, $50, $35, $2A, $5F, $76, $30, $31, $37, $33, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $32, $20, $3D, $20, $64, $6F, $74, $28,
      $5F, $4E, $56, $65, $63, $30, $31, $36, $35, $2C, $20, $5F, $76, $61, $6C, $30, $31, $37, $31, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $53, $6F, $6D, $65,
      $50, $6F, $69, $6E, $74, $30, $31, $36, $35, $20, $3D, $20, $5F, $76, $61, $6C, $30, $31, $37, $31, $20, $2D, $20, $28, $32, $2E, $30, $2A, $5F, $4E, $56, $65, $63, $30, $31, $36, $35, $29, $2A, $5F,
      $54, $4D, $50, $31, $32, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $32, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4E, $56, $65, $63, $30, $31,
      $36, $35, $2C, $20, $5F, $76, $61, $6C, $30, $31, $36, $39, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $4E, $31, $43, $30, $31, $36, $35, $20, $3D, $20, $5F,
      $76, $61, $6C, $30, $31, $36, $39, $20, $2D, $20, $28, $32, $2E, $30, $2A, $5F, $4E, $56, $65, $63, $30, $31, $36, $35, $29, $2A, $5F, $54, $4D, $50, $31, $32, $3B, $0D, $0A, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30, $31, $36, $35, $20, $3D, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30, $31, $36, $35, $20, $2D,
      $20, $28, $5F, $4E, $31, $43, $30, $31, $36, $35, $20, $2D, $20, $5F, $76, $61, $6C, $30, $31, $36, $37, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $53, $6F,
      $6D, $65, $50, $6F, $69, $6E, $74, $30, $31, $36, $35, $2E, $79, $20, $3D, $20, $31, $2E, $30, $20, $2D, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30, $31, $36, $35, $2E, $79, $3B, $0D,
      $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $31, $38, $37, $20, $3D, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30, $31, $36, $35, $3B, $0D,
      $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $76, $61, $6C, $30, $31, $38, $37, $2E, $78, $20, $3D, $20, $5F, $53, $6F, $6D, $65, $50, $6F, $69, $6E, $74, $30, $31, $36, $35,
      $2E, $78, $2F, $5F, $4B, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $36, $34, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28,
      $5F, $42, $61, $63, $6B, $2C, $20, $5F, $76, $61, $6C, $30, $31, $38, $37, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $36, $32, $20, $3D,
      $20, $5F, $54, $4D, $50, $31, $36, $34, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $7D, $20, $65, $6C, $73, $65, $20, $7B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $30, $31, $20, $3D, $20, $5F, $41, $31, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $30, $31, $20, $3D, $20, $5F, $42, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B,
      $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $36, $20, $3D, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $30, $31, $2E, $78, $2A, $5F, $50, $6F, $69,
      $6E, $74, $42, $30, $32, $30, $31, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $30, $31, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $30, $31, $2E, $79, $3B,
      $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $30, $33, $20, $3D, $20, $5F, $42, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45,
      $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $30, $33, $20, $3D, $20, $5F, $43, $50, $6F, $69, $6E,
      $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $5F, $50, $6F, $69, $6E, $74,
      $41, $30, $32, $30, $33, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $30, $33, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $30, $33, $2E, $78, $2A, $5F, $50,
      $6F, $69, $6E, $74, $41, $30, $32, $30, $33, $2E, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $30, $35, $20, $3D, $20,
      $5F, $42, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30,
      $32, $30, $35, $20, $3D, $20, $5F, $43, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54,
      $4D, $50, $38, $20, $3D, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $30, $35, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $30, $35, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69,
      $6E, $74, $42, $30, $32, $30, $35, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $30, $35, $2E, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $50,
      $6F, $69, $6E, $74, $41, $30, $32, $30, $37, $20, $3D, $20, $5F, $43, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $30, $37, $20, $3D, $20, $5F, $41, $31, $50, $6F, $69, $6E, $74, $20, $2D, $20, $54, $45, $58, $30, $2E, $78, $79, $3B, $0D, $0A, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $39, $20, $3D, $20, $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $30, $37, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $42,
      $30, $32, $30, $37, $2E, $79, $20, $2D, $20, $5F, $50, $6F, $69, $6E, $74, $42, $30, $32, $30, $37, $2E, $78, $2A, $5F, $50, $6F, $69, $6E, $74, $41, $30, $32, $30, $37, $2E, $79, $3B, $0D, $0A, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $69, $66, $20, $28, $5F, $54, $4D, $50, $36, $2A, $5F, $54, $4D, $50, $37, $20, $3E, $20, $30, $2E, $30, $20, $26, $26, $20, $5F, $54, $4D, $50,
      $38, $2A, $5F, $54, $4D, $50, $39, $20, $3E, $20, $30, $2E, $30, $29, $20, $7B, $20, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $31,
      $39, $30, $20, $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $54, $61, $72, $67, $65, $74, $2C, $20, $54, $45, $58, $30, $2E, $78, $79, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $36, $32, $20, $3D, $20, $5F, $54, $4D, $50, $31, $39, $30, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $7D, $20, $65, $6C, $73, $65, $20, $7B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $39, $34, $20, $3D, $20, $74,
      $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $54, $45, $58, $30, $2E, $78, $79, $29, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
      $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $36, $32, $20, $3D, $20, $5F, $54, $4D, $50, $31, $39, $34, $3B, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $7D, $20, $2F, $2F,
      $20, $65, $6E, $64, $20, $69, $66, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $7D, $20, $2F, $2F, $20, $65, $6E, $64, $20, $69, $66, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $5F, $52,
      $65, $74, $56, $61, $6C, $20, $3D, $20, $5F, $54, $4D, $50, $31, $36, $32, $3B, $0D, $0A, $20, $20, $20, $20, $7D, $20, $2F, $2F, $20, $65, $6E, $64, $20, $69, $66, $0D, $0A, $20, $20, $20, $20, $67,
      $6C, $5F, $46, $72, $61, $67, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $52, $65, $74, $56, $61, $6C, $3B, $0D, $0A, $20, $20, $20, $20, $72, $65, $74, $75, $72, $6E, $3B, $0D, $0A, $7D, $20, $0D,
      $0A], [
      TContextShaderVariable.Create('APoint', TContextShaderVariableKind.Float2, 0, 1),
      TContextShaderVariable.Create('A1Point', TContextShaderVariableKind.Float2, 0, 1),
      TContextShaderVariable.Create('BPoint', TContextShaderVariableKind.Float2, 0, 1),
      TContextShaderVariable.Create('CPoint', TContextShaderVariableKind.Float2, 0, 1),
      TContextShaderVariable.Create('LineAB', TContextShaderVariableKind.Float2, 0, 1),
      TContextShaderVariable.Create('LineAB1', TContextShaderVariableKind.Float2, 0, 1),
      TContextShaderVariable.Create('K', TContextShaderVariableKind.Float, 0, 1),
      TContextShaderVariable.Create('Len', TContextShaderVariableKind.Float, 0, 1),
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Target', TContextShaderVariableKind.Texture, 1, 0),
      TContextShaderVariable.Create('Back', TContextShaderVariableKind.Texture, 2, 0)]
    )
    {$ENDREGION}

  ]);
end;

class function TSwipeFilter.FilterAttr: TFilterRec;
begin
  Result := TFilterRec.Create('SwipeTransition', 'A swipe transition effect.', [
    TFilterValueRec.Create('MousePoint', 'The point of coursor', TPointF.Create(5,5), TPointF.Zero, TPointF.Create($FFFF,$FFFF)),
    TFilterValueRec.Create('CornerPoint', 'The point of corner', TPointF.Zero, TPointF.Zero, TPointF.Create($FFFF,$FFFF)),
    TFilterValueRec.Create('Deep', 'The deep of fold', 50, 0, 100),
    TFilterValueRec.Create('Target', 'Target desription', TFilterValueType.Bitmap),
    TFilterValueRec.Create('Back', 'Back page texture', TFilterValueType.Bitmap)
    ]);
end;

procedure TSwipeFilter.LoadShaders;
begin
  FilterContext.SetShaders(FVertexShader, FShaders[FPass]);
  FilterContext.SetShaderVariable('APoint',  [Vector3D(FAPoint.X, FAPoint.Y, 0, 0)]);
  FilterContext.SetShaderVariable('A1Point', [Vector3D(FA1Point.X, FA1Point.Y, 0, 0)]);
  FilterContext.SetShaderVariable('BPoint',  [Vector3D(FBPoint.X, FBPoint.Y, 0, 0)]);
  FilterContext.SetShaderVariable('CPoint',  [Vector3D(FCPoint.X, FCPoint.Y, 0, 0)]);
  FilterContext.SetShaderVariable('LineAB',  [Vector3D(FLineAB.X, FLineAB.Y, 0, 0)]);
  FilterContext.SetShaderVariable('LineAB1', [Vector3D(FLineAB1.X, FLineAB1.Y, 0, 0)]);
  FilterContext.SetShaderVariable('K',       [Vector3D(FK, 0, 0, 0)]);
  FilterContext.SetShaderVariable('Len',     [Vector3D(FLength, 0, 0, 0)]);
  inherited;
end;

type
  TReflectionFilter = class(TFilter)
  public
    constructor Create; override;
    class function FilterAttr: TFilterRec; override;
  end;

{ TReflectionFilter }

constructor TReflectionFilter.Create;
begin
  inherited;
  FShaders[0] := TShaderManager.RegisterShaderFromData('Reflection.fps', TContextShaderKind.PixelShader, '', [

    {$REGION 'TContextShaderArch.SKSL'}
    TContextShaderSource.Create(TContextShaderArch.SKSL, [
      $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $32, $20, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $73, $68, $61, $64, $65, $72, 
      $20, $49, $6E, $70, $75, $74, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $4F, $70, $61, $63, $69, $74, $79, $3B, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, 
      $61, $74, $20, $4C, $65, $6E, $67, $74, $68, $3B, $68, $61, $6C, $66, $34, $20, $6D, $61, $69, $6E, $28, $66, $6C, $6F, $61, $74, $32, $20, $61, $29, $7B, $66, $6C, $6F, $61, $74, $34, $20, $62, $3D, 
      $66, $6C, $6F, $61, $74, $34, $28, $49, $6E, $70, $75, $74, $2E, $65, $76, $61, $6C, $28, $66, $6C, $6F, $61, $74, $32, $28, $61, $2E, $78, $2C, $52, $65, $73, $6F, $6C, $75, $74, $69, $6F, $6E, $2E, 
      $79, $2D, $61, $2E, $79, $29, $29, $29, $3B, $62, $2A, $3D, $28, $31, $2E, $2D, $63, $6C, $61, $6D, $70, $28, $28, $61, $2E, $79, $2F, $4C, $65, $6E, $67, $74, $68, $29, $2F, $52, $65, $73, $6F, $6C, 
      $75, $74, $69, $6F, $6E, $2E, $79, $2C, $30, $2E, $2C, $31, $2E, $29, $29, $2A, $4F, $70, $61, $63, $69, $74, $79, $3B, $72, $65, $74, $75, $72, $6E, $20, $68, $61, $6C, $66, $34, $28, $62, $29, $3B, 
      $7D, $0A, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Resolution', TContextShaderVariableKind.Float2, 0, 8),
      TContextShaderVariable.Create('Opacity', TContextShaderVariableKind.Float, 1, 4),
      TContextShaderVariable.Create('Length', TContextShaderVariableKind.Float, 2, 4)]
    ),
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FF, $FF, $FE, $FF, $30, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $97, $00, $00, $00, $00, $02, $FF, $FF, $03, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $90, $00, $00, $00,
      $58, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $60, $00, $00, $00, $00, $00, $00, $00, $70, $00, $00, $00, $02, $00, $01, $00, $01, $00, $00, $00, $78, $00, $00, $00, $00, $00, $00, $00,
      $88, $00, $00, $00, $02, $00, $00, $00, $01, $00, $00, $00, $78, $00, $00, $00, $00, $00, $00, $00, $49, $6E, $70, $75, $74, $00, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00, $01, $00, $00, $00,
      $00, $00, $00, $00, $4C, $65, $6E, $67, $74, $68, $00, $AB, $00, $00, $03, $00, $01, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $4F, $70, $61, $63, $69, $74, $79, $00, $70, $73, $5F, $32,
      $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00,
      $51, $00, $00, $05, $02, $00, $0F, $A0, $00, $00, $00, $00, $00, $00, $80, $3F, $00, $00, $80, $BF, $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02,
      $00, $00, $00, $90, $00, $08, $0F, $A0, $06, $00, $00, $02, $00, $00, $08, $80, $01, $00, $00, $A0, $05, $00, $00, $03, $00, $00, $11, $80, $00, $00, $FF, $80, $00, $00, $55, $B0, $02, $00, $00, $03,
      $00, $00, $01, $80, $00, $00, $00, $81, $02, $00, $55, $A0, $05, $00, $00, $03, $00, $00, $01, $80, $00, $00, $00, $80, $00, $00, $00, $A0, $04, $00, $00, $04, $01, $00, $03, $80, $00, $00, $E4, $B0,
      $02, $00, $C9, $A0, $02, $00, $E4, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $00, $80, $01, $00, $E4, $80,
      $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Length', TContextShaderVariableKind.Float, 1, 1),
      TContextShaderVariable.Create('Opacity', TContextShaderVariableKind.Float, 0, 1)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.DX11_level_9'}
    {$IF defined(MSWindows)}
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $49, $5B, $41, $A6, $F3, $C9, $B6, $BE, $E6, $B9, $1F, $94, $F7, $EB, $5A, $6E, $01, $00, $00, $00, $74, $04, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $28, $01, $00, $00,
      $64, $02, $00, $00, $E0, $02, $00, $00, $0C, $04, $00, $00, $40, $04, $00, $00, $41, $6F, $6E, $39, $E8, $00, $00, $00, $E8, $00, $00, $00, $00, $02, $FF, $FF, $B4, $00, $00, $00, $34, $00, $00, $00,
      $01, $00, $28, $00, $00, $00, $34, $00, $00, $00, $34, $00, $01, $00, $24, $00, $00, $00, $34, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $02, $FF, $FF,
      $51, $00, $00, $05, $01, $00, $0F, $A0, $00, $00, $80, $3F, $00, $00, $80, $BF, $00, $00, $00, $00, $00, $00, $00, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02,
      $00, $00, $00, $90, $00, $08, $0F, $A0, $06, $00, $00, $02, $00, $00, $08, $80, $00, $00, $55, $A0, $05, $00, $00, $03, $00, $00, $11, $80, $00, $00, $FF, $80, $00, $00, $55, $B0, $02, $00, $00, $03,
      $00, $00, $01, $80, $00, $00, $00, $81, $01, $00, $00, $A0, $05, $00, $00, $03, $00, $00, $01, $80, $00, $00, $00, $80, $00, $00, $00, $A0, $04, $00, $00, $04, $01, $00, $03, $80, $00, $00, $E4, $B0,
      $01, $00, $E4, $A0, $01, $00, $D2, $A0, $42, $00, $00, $03, $01, $00, $0F, $80, $01, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $00, $80, $01, $00, $E4, $80,
      $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00, $53, $48, $44, $52, $34, $01, $00, $00, $40, $00, $00, $00, $4D, $00, $00, $00, $59, $00, $00, $04, $46, $8E, $20, $00,
      $00, $00, $00, $00, $01, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $00, $00, $00, $00, $58, $18, $00, $04, $00, $70, $10, $00, $00, $00, $00, $00, $55, $55, $00, $00, $62, $10, $00, $03,
      $32, $10, $10, $00, $00, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00, $00, $00, $00, $00, $68, $00, $00, $02, $02, $00, $00, $00, $0E, $20, $00, $08, $12, $00, $10, $00, $00, $00, $00, $00,
      $1A, $10, $10, $00, $00, $00, $00, $00, $1A, $80, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $08, $12, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $80, $41, $00, $00, $00,
      $00, $00, $00, $00, $01, $40, $00, $00, $00, $00, $80, $3F, $38, $00, $00, $08, $12, $00, $10, $00, $00, $00, $00, $00, $0A, $00, $10, $00, $00, $00, $00, $00, $0A, $80, $20, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $32, $00, $00, $0F, $62, $00, $10, $00, $00, $00, $00, $00, $06, $11, $10, $00, $00, $00, $00, $00, $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $80, $3F, $00, $00, $80, $BF,
      $00, $00, $00, $00, $02, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $80, $3F, $00, $00, $00, $00, $45, $00, $00, $09, $F2, $00, $10, $00, $01, $00, $00, $00, $96, $05, $10, $00,
      $00, $00, $00, $00, $46, $7E, $10, $00, $00, $00, $00, $00, $00, $60, $10, $00, $00, $00, $00, $00, $38, $00, $00, $07, $F2, $20, $10, $00, $00, $00, $00, $00, $06, $00, $10, $00, $00, $00, $00, $00,
      $46, $0E, $10, $00, $01, $00, $00, $00, $3E, $00, $00, $01, $53, $54, $41, $54, $74, $00, $00, $00, $07, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00, $02, $00, $00, $00, $04, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $52, $44, $45, $46, $24, $01, $00, $00, $01, $00, $00, $00, $8C, $00, $00, $00, $03, $00, $00, $00, $1C, $00, $00, $00,
      $00, $04, $FF, $FF, $00, $11, $00, $00, $F3, $00, $00, $00, $7C, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00,
      $00, $00, $00, $00, $7C, $00, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $01, $00, $00, $00, $0C, $00, $00, $00, $82, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $49, $6E, $70, $75, $74, $00, $24, $47, $6C, $6F, $62, $61,
      $6C, $73, $00, $AB, $82, $00, $00, $00, $02, $00, $00, $00, $A4, $00, $00, $00, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $D4, $00, $00, $00, $00, $00, $00, $00, $04, $00, $00, $00,
      $02, $00, $00, $00, $DC, $00, $00, $00, $00, $00, $00, $00, $EC, $00, $00, $00, $04, $00, $00, $00, $04, $00, $00, $00, $02, $00, $00, $00, $DC, $00, $00, $00, $00, $00, $00, $00, $4F, $70, $61, $63,
      $69, $74, $79, $00, $00, $00, $03, $00, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $4C, $65, $6E, $67, $74, $68, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29,
      $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E, $39, $35, $32, $2E, $32, $38, $34, $34, $00, $49, $53, $47, $4E,
      $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $03, $03, $00, $00, $54, $45, $58, $43,
      $4F, $4F, $52, $44, $00, $AB, $AB, $AB, $4F, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00,
      $00, $00, $00, $00, $0F, $00, $00, $00, $53, $56, $5F, $54, $61, $72, $67, $65, $74, $00, $AB, $AB], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Opacity', TContextShaderVariableKind.Float, 0, 4),
      TContextShaderVariable.Create('Length', TContextShaderVariableKind.Float, 4, 4)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.Metal'}
    {$IF defined(MACOS)}
    TContextShaderSource.Create(
      TContextShaderArch.Metal,
      TEncoding.UTF8.GetBytes(
        'using namespace metal;'+

        'struct ProjectedVertex {'+
          'float4 position [[position]];'+
          'float2 textureCoord;'+
        '};'+

        'fragment float4 fragmentShader(const ProjectedVertex in [[stage_in]],'+
                                       'constant float4 &Opacity [[buffer(0)]],'+
                                       'constant float4 &Length [[buffer(1)]],'+
                                       'const texture2d<float> Input [[texture(0)]],'+
                                       'const sampler InputSampler [[sampler(0)]]) {'+
          'float4 color;'+
          'color = Input.sample(InputSampler, float2(in.textureCoord.x, 1-in.textureCoord.y));'+
          'color *= (1 - clamp(in.textureCoord.y / Length.x, 0.0f, 1.0f)) * Opacity.x;'+
          'return color;'+
        '}'
      ),
      [TContextShaderVariable.Create('Opacity', TContextShaderVariableKind.Float, 0, 1),
       TContextShaderVariable.Create('Length', TContextShaderVariableKind.Float, 1, 1),
       TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0)]
    ),
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'TContextShaderArch.GLSL'}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $66, $6C, $6F, $61, $74, $20, $5F, $54, $4D, $50, $31, $3B, $0D, $0A, $76, $65, $63, $32, $20, $5F,
      $63, $30, $30, $30, $36, $3B, $0D, $0A, $66, $6C, $6F, $61, $74, $20, $5F, $54, $4D, $50, $37, $3B, $0D, $0A, $66, $6C, $6F, $61, $74, $20, $5F, $78, $30, $30, $30, $38, $3B, $0D, $0A, $75, $6E, $69,
      $66, $6F, $72, $6D, $20, $73, $61, $6D, $70, $6C, $65, $72, $32, $44, $20, $5F, $49, $6E, $70, $75, $74, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $5F, $4F,
      $70, $61, $63, $69, $74, $79, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $66, $6C, $6F, $61, $74, $20, $5F, $4C, $65, $6E, $67, $74, $68, $3B, $0D, $0A, $76, $6F, $69, $64, $20, $6D, $61,
      $69, $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20, $20, $76, $65, $63, $34, $20, $5F, $63, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $63, $30, $30, $30, $36, $20, $3D, $20,
      $76, $65, $63, $32, $28, $54, $45, $58, $30, $2E, $78, $2C, $20, $31, $2E, $30, $20, $2D, $20, $54, $45, $58, $30, $2E, $79, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $63, $6F, $6C, $6F, $72, $20,
      $3D, $20, $74, $65, $78, $74, $75, $72, $65, $32, $44, $28, $5F, $49, $6E, $70, $75, $74, $2C, $20, $5F, $63, $30, $30, $30, $36, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $78, $30, $30, $30, $38,
      $20, $3D, $20, $28, $31, $2E, $30, $20, $2D, $20, $54, $45, $58, $30, $2E, $79, $29, $2F, $5F, $4C, $65, $6E, $67, $74, $68, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $31, $20, $3D, $20,
      $6D, $69, $6E, $28, $31, $2E, $30, $2C, $20, $5F, $78, $30, $30, $30, $38, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $54, $4D, $50, $37, $20, $3D, $20, $6D, $61, $78, $28, $30, $2E, $30, $2C, $20,
      $5F, $54, $4D, $50, $31, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $63, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $63, $6F, $6C, $6F, $72, $2A, $76, $65, $63, $34, $28, $28, $31, $2E, $30, $20, $2D,
      $20, $5F, $54, $4D, $50, $37, $29, $2A, $5F, $4F, $70, $61, $63, $69, $74, $79, $2C, $20, $28, $31, $2E, $30, $20, $2D, $20, $5F, $54, $4D, $50, $37, $29, $2A, $5F, $4F, $70, $61, $63, $69, $74, $79,
      $2C, $20, $28, $31, $2E, $30, $20, $2D, $20, $5F, $54, $4D, $50, $37, $29, $2A, $5F, $4F, $70, $61, $63, $69, $74, $79, $2C, $20, $28, $31, $2E, $30, $20, $2D, $20, $5F, $54, $4D, $50, $37, $29, $2A,
      $5F, $4F, $70, $61, $63, $69, $74, $79, $29, $3B, $0D, $0A, $20, $20, $20, $20, $67, $6C, $5F, $46, $72, $61, $67, $43, $6F, $6C, $6F, $72, $20, $3D, $20, $5F, $63, $6F, $6C, $6F, $72, $3B, $0D, $0A,
      $20, $20, $20, $20, $72, $65, $74, $75, $72, $6E, $3B, $0D, $0A, $7D, $20, $0D, $0A], [
      TContextShaderVariable.Create('Input', TContextShaderVariableKind.Texture, 0, 0),
      TContextShaderVariable.Create('Opacity', TContextShaderVariableKind.Float, 0, 1),
      TContextShaderVariable.Create('Length', TContextShaderVariableKind.Float, 0, 1)]
    )
    {$ENDREGION}

  ]);
end;

class function TReflectionFilter.FilterAttr: TFilterRec;
begin
  Result := TFilterRec.Create('ReflectionFilter', 'An reflection effect.', [
    TFilterValueRec.Create('Opacity', 'The opacity of result image.', 1, 0, 1),
    TFilterValueRec.Create('Length', 'The length of reflection.', 0.5, 0, 1)]
  );
end;


initialization
  TFilterManager.RegisterFilter('Geometry', TAffineFilter);
  TFilterManager.RegisterFilter('Geometry', TPerspectiveFilter);
  TFilterManager.RegisterFilter('Geometry', TCropFilter);
  TFilterManager.RegisterFilter('Blur', TGaussianBlurFilter);
  TFilterManager.RegisterFilter('Style', TGlowFilter);
  TFilterManager.RegisterFilter('Style', TInnerGlowFilter);
  TFilterManager.RegisterFilter('Style', TReflectionFilter);
  TFilterManager.RegisterFilter('Transition', TSwipeFilter);
end.
