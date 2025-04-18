{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Types3D;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.Types, System.UITypes, System.UIConsts,
  System.Messaging, System.Math.Vectors, FMX.Types, FMX.Graphics;

{ Points and rects }

type
  /// <summary>Record type for the information of an axis aligned box in
  /// 3D.</summary>
  TBoundingBox = record
  private
    function GetWidth: Single;
    procedure SetWidth(const Value: Single);
    function GetHeight: Single;
    procedure SetHeight(const Value: Single);
    function GetDepth: Single;
    procedure SetDepth(const Value: Single);
    //returns the center point of the box;
    function GetCenterPoint: TPoint3D;
  public
    /// <summary>Constructor with a simple vertex.</summary>
    constructor Create(const AnOrigin: TPoint3D); overload;
    /// <summary>Constructor with a vertex and the dimensions of the box.</summary>
    constructor Create(const AnOrigin: TPoint3D; const Width, Height, Depth: Single); overload;
    /// <summary>Constructor with the corners values.</summary>
    constructor Create(const Left, Top, Near, Right, Bottom, Far: Single); overload;
    /// <summary>Constructor with two corners. See Normalize function.</summary>
    constructor Create(const APoint1, APoint2: TPoint3D; NormalizeBox: Boolean = False); overload;
    /// <summary>Constructor with a reference box. See Normalize function.</summary>
    constructor Create(const ABox: TBoundingBox; NormalizeBox: Boolean = False); overload;
    /// <summary>Constructor with a point cloud.</summary>
    constructor Create(const Points: TArray<TPoint3D>); overload;
    /// <summary>Constructor with a point cloud.</summary>
    constructor Create(const Points: PPoint3D; const PointCount: Integer); overload;

    /// <summary>Equality operator taking account a default epsilon.</summary>
    class operator Equal(const LeftBox, RightBox: TBoundingBox): Boolean;
    /// <summary>Not equality operator taking account a default epsilon.</summary>
    class operator NotEqual(const LeftBox, RightBox: TBoundingBox): Boolean;

    /// <summary>Union of two boxes.</summary>
    class operator Add(const LeftBox, RightBox: TBoundingBox): TBoundingBox;

    /// <summary>Intersects two boxes.</summary>
    class operator Multiply(const LeftBox, RightBox: TBoundingBox): TBoundingBox;

    /// <summary>Returns true if the box is invalid.</summary>
    class function Empty: TBoundingBox; inline; static;

    /// <summary>This function returns the scale value that can be used to scale this box to fit into the desired area.</summary>
    function FitIntoScale(const ADesignatedArea: TBoundingBox): Single;
    /// <summary>This function returns a new box which is the original box scaled by a single value to fit into
    /// the desired area. A ARatio value is used to return the scale value used to perform this operation.</summary>
    function FitInto(const ADesignatedArea: TBoundingBox; out ARatio: Single): TBoundingBox; overload;
    /// <summary>This function returns a new box which is the original box scaled by a single value to fit into
    /// the desired area.</summary>
    function FitInto(const ADesignatedArea: TBoundingBox): TBoundingBox; overload;

    /// <summary>Makes sure TopLeftNear is above and to the left of BottomRightFar.</summary>
    function Normalize: TBoundingBox;

    /// <summary>Returns true if left = right or top = bottom or near = far.</summary>
    function IsEmpty(const Epsilon: Single = TEpsilon.Vector): Boolean;

    /// <summary>Returns true if the point is inside the box.</summary>
    function Contains(const APoint: TPoint3D): Boolean; overload;

    /// <summary>Returns true if the box encloses ABox completely.</summary>
    function Contains(const ABox: TBoundingBox): Boolean; overload;

    /// <summary>Returns true if any part of the box covers ABox.</summary>
    function IntersectsWith(const ABox: TBoundingBox): Boolean;

    /// <summary>Computes an intersection with the incoming box and returns that intersection.</summary>
    function Intersect(const DestBox: TBoundingBox): TBoundingBox;

    /// <summary>Returns the minimum box (with lowest volume) envolving this box and DestBox.</summary>
    function Union(const DestBox: TBoundingBox): TBoundingBox; overload;

    /// <summary>Offsets the box origin relative to current position.</summary>
    function Offset(const DX, DY, DZ: Single): TBoundingBox; overload;
    /// <summary>Offsets the box origin relative to current position.</summary>
    function Offset(const APoint: TPoint3D): TBoundingBox; overload;

    /// <summary>Inflate the box by DX, DY and DZ.</summary>
    function Inflate(const DX, DY, DZ: Single): TBoundingBox; overload;
    /// <summary>Inflate in all directions.</summary>
    function Inflate(const DL, DT, DN, DR, DB, DF: Single): TBoundingBox; overload;

    /// <summary>Returns the size of the box in a TPoint record.</summary>
    function GetSize: TPoint3D;

    /// <summary>The same as the equality operator, taking account a given epsilon.</summary>
    function EqualsTo(const ABox: TBoundingBox; const Epsilon: Single = 0): Boolean;

    /// <summary>When the Width value is changed, the Right value is modified, leaving the Left value
    /// unchanged.</summary>
    property Width: Single read GetWidth write SetWidth;
    /// <summary>When the Height value is changed, the Bottom value is modified, leaving the Top value
    /// unchanged.</summary>
    property Height: Single read GetHeight write SetHeight;
    /// <summary>When the Depth value is changed, the Far value is modified, leaving the Near value unchanged.</summary>
    property Depth: Single read GetDepth write SetDepth;

    /// <summary>Returns the center of the box.</summary>
    property CenterPoint: TPoint3D read GetCenterPoint;

    /// <summary>Case to couple the fields in the box.</summary>
    case Integer of
      /// <summary>Minimum and maximum corners by separated values.</summary>
      0: (Left, Top, Near, Right, Bottom, Far: Single;);
      /// <summary>Minimum and maximum corners.</summary>
      1: (TopLeftNear, BottomRightFar: TPoint3D);
      /// <summary>Minimum and maximum corners.</summary>
      2: (MinCorner, MaxCorner: TPoint3D);
  end;

  TBox = TBoundingBox deprecated 'Use TBoundingBox';
  TMatrix3DDynArray = array of TMatrix3D;
  TPoint3DDynArray = array of TPoint3D;
  TPointFDynArray = array of TPointF;

const
  NullVector3D: TVector3D = (X: 0; Y: 0; Z: 0; W: 1);
  NullPoint3D: TPoint3D = (X: 0; Y: 0; Z: 0);

  MaxLightCount = 256;

type
  TContext3D = class;
  TVertexBuffer = class;
  TIndexBuffer = class;
  TContextShader = class;
  TMaterial = class;

{ TVertexBuffer }

  TVertexFormat = (Vertex, Normal, Color0, Color1, Color2, Color3, ColorF0, ColorF1, ColorF2, ColorF3, TexCoord0, TexCoord1, TexCoord2, TexCoord3, BiNormal, Tangent);
  TVertexFormats = set of TVertexFormat;

  TVertexElement = record
    Format: TVertexFormat;
    Offset: Integer;
  end;
  TVertexDeclaration = array of TVertexElement;

  TVertexBuffer = class(TPersistent)
  private
    FBuffer: Pointer;
    FFormat: TVertexFormats;
    FLength: Integer;
    FSize: Integer;
    FVertexSize: Integer;
    FTexCoord0: Integer;
    FTexCoord1: Integer;
    FTexCoord2: Integer;
    FTexCoord3: Integer;
    FColor0: Integer;
    FColor1: Integer;
    FColor2: Integer;
    FColor3: Integer;
    FColorF0: Integer;
    FColorF1: Integer;
    FColorF2: Integer;
    FColorF3: Integer;
    FNormal: Integer;
    FBiNormal: Integer;
    FTangent: Integer;
    FSaveLength: Integer;
    function GetVertices(AIndex: Integer): TPoint3D; inline;
    function GetTexCoord0(AIndex: Integer): TPointF; inline;
    function GetColor0(AIndex: Integer): TAlphaColor; inline;
    function GetNormals(AIndex: Integer): TPoint3D; inline;
    function GetNormalsPtr(AIndex: Integer): PPoint3D; inline;
    function GetColor1(AIndex: Integer): TAlphaColor; inline;
    function GetTexCoord1(AIndex: Integer): TPointF; inline;
    function GetTexCoord2(AIndex: Integer): TPointF; inline;
    function GetTexCoord3(AIndex: Integer): TPointF; inline;
    function GetVerticesPtr(AIndex: Integer): PPoint3D; inline;
    function GetItemPtr(AIndex: Integer): Pointer; inline;
    procedure SetVertices(AIndex: Integer; const Value: TPoint3D); inline;
    procedure SetColor0(AIndex: Integer; const Value: TAlphaColor); inline;
    procedure SetNormals(AIndex: Integer; const Value: TPoint3D); inline;
    procedure SetColor1(AIndex: Integer; const Value: TAlphaColor); inline;
    procedure SetTexCoord0(AIndex: Integer; const Value: TPointF); inline;
    procedure SetTexCoord1(AIndex: Integer; const Value: TPointF); inline;
    procedure SetTexCoord2(AIndex: Integer; const Value: TPointF); inline;
    procedure SetTexCoord3(AIndex: Integer; const Value: TPointF); inline;
    procedure SetLength(const Value: Integer);
    function GetColor2(AIndex: Integer): TAlphaColor;
    function GetColor3(AIndex: Integer): TAlphaColor;
    procedure SetColor2(AIndex: Integer; const Value: TAlphaColor);
    procedure SetColor3(AIndex: Integer; const Value: TAlphaColor);
    function GetBiNormals(AIndex: Integer): TPoint3D;
    procedure SetBiNormals(AIndex: Integer; const Value: TPoint3D);
    function GetTangents(AIndex: Integer): TPoint3D;
    procedure SetTangents(AIndex: Integer; const Value: TPoint3D);
    function GetBiNormalsPtr(AIndex: Integer): PPoint3D;
    function GetTangentsPtr(AIndex: Integer): PPoint3D;
    procedure SetFormat(Value: TVertexFormats);
  protected
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(const AFormat: TVertexFormats; const ALength: Integer); virtual;
    destructor Destroy; override;
    procedure BeginDraw(const ALength: Integer);
    procedure EndDraw;
    function GetVertexDeclarations: TVertexDeclaration;
    property Buffer: Pointer read FBuffer;
    property Size: Integer read FSize;
    property VertexSize: Integer read FVertexSize;
    property Length: Integer read FLength write SetLength;
    property Format: TVertexFormats read FFormat;
    { items access }
    property ItemPtr[AIndex: Integer]: Pointer read GetItemPtr;
    property Vertices[AIndex: Integer]: TPoint3D read GetVertices write SetVertices;
    property VerticesPtr[AIndex: Integer]: PPoint3D read GetVerticesPtr;
    property Normals[AIndex: Integer]: TPoint3D read GetNormals write SetNormals;
    property NormalsPtr[AIndex: Integer]: PPoint3D read GetNormalsPtr;
    property BiNormals[AIndex: Integer]: TPoint3D read GetBiNormals write SetBiNormals;
    property BiNormalsPtr[AIndex: Integer]: PPoint3D read GetBiNormalsPtr;
    property Tangents[AIndex: Integer]: TPoint3D read GetTangents write SetTangents;
    property TangentsPtr[AIndex: Integer]: PPoint3D read GetTangentsPtr;
    property Color0[AIndex: Integer]: TAlphaColor read GetColor0 write SetColor0;
    property Color1[AIndex: Integer]: TAlphaColor read GetColor1 write SetColor1;
    property Color2[AIndex: Integer]: TAlphaColor read GetColor2 write SetColor2;
    property Color3[AIndex: Integer]: TAlphaColor read GetColor3 write SetColor3;
    property TexCoord0[AIndex: Integer]: TPointF read GetTexCoord0 write SetTexCoord0;
    property TexCoord1[AIndex: Integer]: TPointF read GetTexCoord1 write SetTexCoord1;
    property TexCoord2[AIndex: Integer]: TPointF read GetTexCoord2 write SetTexCoord2;
    property TexCoord3[AIndex: Integer]: TPointF read GetTexCoord3 write SetTexCoord3;
  end;

{ TIndexBuffer }

  TIndexFormat = (UInt16, UInt32);

  TIndexBuffer = class(TPersistent)
  private
    FBuffer: Pointer;
    FLength: Integer;
    FIndexSize: Integer;
    FSize: Integer;
    FSaveLength: Integer;
    FFormat: TIndexFormat;
    function GetIndices(AIndex: Integer): Integer; inline;
    procedure SetIndices(AIndex: Integer; const Value: Integer); inline;
    procedure SetLength(const Value: Integer);
    procedure SetFormat(const Value: TIndexFormat);
  protected
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(const ALength: Integer; const AFormat: TIndexFormat = TIndexFormat.UInt16); virtual;
    destructor Destroy; override;
    procedure BeginDraw(const ALength: Integer);
    procedure EndDraw;
    property Buffer: Pointer read FBuffer;
    property Format: TIndexFormat read FFormat write SetFormat;
    property IndexSize: Integer read FIndexSize;
    property Size: Integer read FSize;
    property Length: Integer read FLength write SetLength;
    { items access }
    property Indices[AIndex: Integer]: Integer read GetIndices write SetIndices; default;
  end;

{ TMeshData }

  TMeshVertex = packed record
    x, y, z: single;
    nx, ny, nz: single;
    tu, tv: single;
  end;

  TMeshData = class(TPersistent)
  public
  type
    TCalculateNormalMethod = (Default, Fastest, Slowest);
  private
  type
    TVertexSmoothNormalInfo = record
      VertexId: Integer;
      ScaledRoundedX, ScaledRoundedY, ScaledRoundedZ: Integer;
    end;
  private
    FVertexBuffer: TVertexBuffer;
    FIndexBuffer: TIndexBuffer;
    FOnChanged: TNotifyEvent;
    FFaceNormals: TPoint3DDynArray;
    FBoundingBox: TBoundingBox;
    FBoundingBoxUpdateNeeded: Boolean;
    function GetNormals: string;
    function GetPoint3Ds: string;
    function GetTexCoordinates: string;
    procedure SetNormals(const Value: string);
    procedure SetPoint3Ds(const Value: string);
    procedure SetTexCoordinates(const Value: string);
    function GetTriangleIndices: string;
    procedure SetTriangleIndices(const Value: string);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadMesh(Stream: TStream);
    procedure WriteMesh(Stream: TStream);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignFromMeshVertex(const Vertices: array of TMeshVertex; const Indices: array of Word); overload;
    procedure AssignFromMeshVertex(const Vertices: array of TMeshVertex; const Indices: array of Cardinal); overload;
    procedure ChangeFormat(const ANewFormat: TVertexFormats);
    procedure Clear;
    procedure CalcFaceNormals(const PropagateFaceNormalsToVertices: Boolean = True);
    procedure CalcSmoothNormals(const Method: TCalculateNormalMethod = TCalculateNormalMethod.Default; const WeldEpsilon: Single = 0.001);
    procedure CalcTangentBinormals;
    /// <summary>Returns the bounding box of the mesh.</summary>
    function GetBoundingBox: TBoundingBox;
    /// <summary>This function flags the mesh to inform it that it should recalculate its new bounding box because some change could be performed.</summary>
    procedure BoundingBoxNeedsUpdate;
    function RayCastIntersect(const Width, Height, Depth: Single; const RayPos, RayDir: TPoint3D;
      var Intersection: TPoint3D): Boolean; overload;
    function RayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean; overload;
    procedure Render(const AContext: TContext3D; const AMaterial: TMaterial; const AOpacity: Single);
    property FaceNormals: TPoint3DDynArray read FFaceNormals;
    property IndexBuffer: TIndexBuffer read FIndexBuffer;
    property VertexBuffer: TVertexBuffer read FVertexBuffer;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Normals: string read GetNormals write SetNormals stored False;
    property Points: string read GetPoint3Ds write SetPoint3Ds stored False;
    property TexCoordinates: string read GetTexCoordinates write SetTexCoordinates stored False;
    property TriangleIndices: string read GetTriangleIndices write SetTriangleIndices stored False;
  end;

{ Shaders }

  TContextShaderKind = (VertexShader, PixelShader);

  TContextShaderVariableKind = (Float, Float2, Float3, Vector, Matrix, Texture);

  TContextShaderVariable = record
    Name: string;
    Kind: TContextShaderVariableKind;
    Index: Integer;
    Size: Integer;
    // filled at run-time
    ShaderKind: TContextShaderKind;
    TextureUnit: Integer;
    constructor Create(const Name: string; const Kind: TContextShaderVariableKind; const Index, Size: Integer);
  end;

  TContextShaderArch = (Undefined, DX9, DX10, DX11_level_9, DX11, Metal, GLSL, Mac, IOS, Android, SKSL);

  TContextShaderCode = array of Byte;
  TContextShaderVariables = array of TContextShaderVariable;

  TContextShaderSource = record
    Arch: TContextShaderArch;
    Code: TContextShaderCode;
    Variables: TContextShaderVariables;
    constructor Create(const Arch: TContextShaderArch; const ACode: array of Byte;
      const AVariables: array of TContextShaderVariable);
    function IsDefined: Boolean;
    function FindVariable(const AName: string; out AShaderVariable: TContextShaderVariable): Boolean;
  end;

  TContextShaderHandle = type THandle;

  TContextShader = class sealed
  private
    FOriginalSource: string;
    FSources: array of TContextShaderSource;
    FHandle: TContextShaderHandle;
    FKind: TContextShaderKind;
    FName: string;
    FRefCount: Integer;
    FContextLostId: TMessageSubscriptionId;
    procedure ContextLostHandler(const Sender : TObject; const Msg : TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    class function BuildKey(const Name: string; const Kind: TContextShaderKind;
      const Sources: array of TContextShaderSource): string;
    function GetSourceByArch(Arch: TContextShaderArch): TContextShaderSource;
    procedure LoadFromData(const Name: string; const Kind: TContextShaderKind;
      const OriginalSource: string; const Sources: array of TContextShaderSource);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(const AStream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(const AStream: TStream);
    property Kind: TContextShaderKind read FKind;
    property Name: string read FName;
    property OriginalSource: string read FOriginalSource;
    property Handle: TContextShaderHandle read FHandle write FHandle;
  end;

  TShaderManager = class sealed
  strict private
    class var FShaderList: TObjectDictionary<string, TContextShader>;
    class function GetShader(const Key: string): TContextShader; static;
  private
  public
    // Reserved for internal use only - do not call directly!
    class procedure UnInitialize;
    // Register shader
    class function RegisterShader(const Shader: TContextShader): TContextShader;
    // Create shader from Data and Register, return already registered if exists
    class function RegisterShaderFromData(const Name: string; const Kind: TContextShaderKind;
      const OriginalSource: string; const Sources: array of TContextShaderSource): TContextShader;
    // Create shader from file and Register, return already registered if exists
    class function RegisterShaderFromFile(const FileName: string): TContextShader;
    // Unregister shader
    class procedure UnregisterShader(const Shader: TContextShader);
  end;

{ Texture }

  ITextureAccess = interface
  ['{3A41B87B-99E6-4DF7-BA7D-CAC558AD0D90}']
    procedure SetHandle(const AHandle: THandle);
    procedure SetTextureScale(const Scale: Single);
    property Handle: THandle write SetHandle;
    property TextureScale: Single write SetTextureScale;
  end;

  TTextureHandle = THandle;

  TTextureFilter = (Nearest, Linear);

  TTextureStyle = (MipMaps, Dynamic, RenderTarget, Volatile);

  TTextureStyles = set of TTextureStyle;

  TTexture = class(TInterfacedPersistent, ITextureAccess)
  private
    FWidth: Integer;
    FHeight: Integer;
    FPixelFormat: TPixelFormat;
    FHandle: TTextureHandle;
    FStyle: TTextureStyles;
    FMagFilter: TTextureFilter;
    FMinFilter: TTextureFilter;
    FTextureScale: Single;
    FRequireInitializeAfterLost: Boolean;
    FBits: Pointer;
    FContextLostId: TMessageSubscriptionId;
    FContextResetId: TMessageSubscriptionId;
    procedure ContextLostHandler(const Sender : TObject; const Msg : TMessage);
    procedure ContextResetHandler(const Sender : TObject; const Msg : TMessage);
    procedure SetPixelFormat(const Value: TPixelFormat);
    procedure SetStyle(const Value: TTextureStyles);
    function GetBytesPerPixel: Integer;
    procedure SetMagFilter(const Value: TTextureFilter);
    procedure SetMinFilter(const Value: TTextureFilter);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    { ITextureAccess }
    procedure SetHandle(const AHandle: THandle);
    procedure SetTextureScale(const Scale: Single);
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetSize(const AWidth, AHeight: Integer);
    function IsEmpty: Boolean;
    { hardware }
    procedure Initialize;
    procedure Finalize;
    { access }
    procedure LoadFromStream(const Stream: TStream);
    procedure UpdateTexture(const Bits: Pointer; const Pitch: Integer);
    { properties }
    property BytesPerPixel: Integer read GetBytesPerPixel;
    property MinFilter: TTextureFilter read FMinFilter write SetMinFilter;
    property MagFilter: TTextureFilter read FMagFilter write SetMagFilter;
    property PixelFormat: TPixelFormat read FPixelFormat write SetPixelFormat;
    property TextureScale: Single read FTextureScale; // hi resolution mode
    property Style: TTextureStyles read FStyle write SetStyle;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Handle: TTextureHandle read FHandle;
  end;

  TTextureBitmap = class(TBitmap)
  private
    FTexture: TTexture;
    function GetTexture: TTexture;
  protected
    procedure DestroyResources; override;
    procedure BitmapChanged; override;
  public
    property Texture: TTexture read GetTexture;
  end;

{ Materials }

  TMaterial = class abstract
  public type
    TProperty = (ModelViewProjection, ModelView, ModelViewInverseTranspose);
  private
    FOnChange: TNotifyEvent;
    FModified: Boolean;
    FNotifyList: TList<Pointer>;
  protected
    procedure DoInitialize; virtual; abstract;
    procedure DoApply(const Context: TContext3D); virtual;
    procedure DoReset(const Context: TContext3D); virtual;
    procedure DoChange; virtual;
    class function DoGetMaterialProperty(const Prop: TMaterial.TProperty): string; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class function GetMaterialProperty(const Prop: TProperty): string;
    procedure Apply(const Context: TContext3D);
    procedure Reset(const Context: TContext3D);
    { FreeNotify }
    procedure AddFreeNotify(const AObject: IFreeNotification);
    procedure RemoveFreeNotify(const AObject: IFreeNotification);
    { Proeprties }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Modified: Boolean read FModified;
  end;
  TMaterialClass = class of TMaterial;

{ Lights }

  TLightType = (Directional, Point, Spot);

  TLightDescription = record
    Enabled: Boolean;
    Color: TAlphaColor;
    LightType: TLightType;
    SpotCutOff: Single;
    SpotExponent: Single;
    Position: TPoint3D;
    Direction: TPoint3D;
    constructor Create(AEnabled: Boolean; AColor: TAlphaColor; ALightType: TLightType; ASpotCutOff: Single;
      ASpotExponent: Single; APosition: TPoint3D; ADirection: TPoint3D);
  end;

  TLightDescriptionList = TList<TLightDescription>;

{ Context's Messages }

  /// <summary>This message is sent when before TContextLostMessage message in order to save data.</summary>
  TContextBeforeLosingMessage = class(TMessage)
  end;

  /// <summary>Message that indicates that the rendering context has been
  /// lost.</summary>
  TContextLostMessage = class(TMessage)
  end;

  /// <summary>Message that indicates that a rendering context has been
  /// created.</summary>
  TContextResetMessage = class(TMessage)
  end;

  /// <summary>Message that indicates that the rendering context has been
  /// removed.</summary>
  TContextRemovedMessage = class(TMessage)
  end;

{ Context }

  TProjection = (Camera, Screen);

  TMultisample = (None, TwoSamples, FourSamples);

  TClearTarget = (Color, Depth, Stencil);

  TClearTargets = set of TClearTarget;

  TStencilOp = (Keep, Zero, Replace, Increase, Decrease, Invert);

  TStencilFunc = (Never, Less, Lequal, Greater, Gequal, Equal, NotEqual, Always);

  TContextState = (
    // 2D screen matrix
    cs2DScene,
    // 3D camera matrix
    cs3DScene,
    // Depth
    csZTestOn, csZTestOff,
    csZWriteOn, csZWriteOff,
    // Alpha Blending
    csAlphaBlendOn, csAlphaBlendOff,
    // Stencil
    csStencilOn, csStencilOff,
    // Color
    csColorWriteOn, csColorWriteOff,
    // Scissor
    csScissorOn, csScissorOff,
    // Faces
    csFrontFace, csBackFace, csAllFace
  );

  TPrimitivesKind = (Points, Lines, Triangles);

  IContextObject = interface
  ['{A78019E4-F09A-4F8D-AC43-E8D51FE3AD69}']
    function GetContext: TContext3D;
    property Context: TContext3D read GetContext;
  end;

  EContext3DException = class(Exception);

  TContextStyle = (RenderTargetFlipped, Fragile);
  TContextStyles = set of TContextStyle;

  TContext3D = class abstract(TInterfacedPersistent, IFreeNotification)
  public type
    TIndexBufferSupport = (Unknown, Int16, Int32);
  protected const
    DefaultMaxLightCount = 8;
    DefaultTextureUnitCount = 8;
    DefaultScale = 1;
    MaxInt16Vertices = 65536;
    MaxInt16Indices = 65536;
  private type
    TStatesArray = array [TContextState] of Boolean;
    TContextStates = record
      States: TStatesArray;
      Matrix: TMatrix3D;
      Context: TContext3D;
      ScissorRect: TRect;
    end;
  private class var
    FContextCount: Integer;
    FSaveStates: TList<TContextStates>;
    FGlobalBeginSceneCount: Integer;
    FChangeStateCount: Integer;
    FChangeShaderCount: Integer;
    FFPS, FRenderTime, FBeginTime, FEndTime: Double;
    FTimerService: IFMXTimerService;
    FFrameCount: Integer;
    FCurrentContext: TContext3D;
    FCurrentStates: TStatesArray;
    FCurrentVertexShader: TContextShader;
    FCurrentPixelShader: TContextShader;
    FCurrentOpacity: Single;
    FCurrentMaterial: TMaterial;
    FCurrentMaterialClass: TMaterialClass;
    FCurrentFormat: TVertexFormats;
    FCurrentScissorRect: TRect;
  private
    FBeginSceneCount: Integer;
    FRecalcScreenMatrix, FRecalcProjectionMatrix: Boolean;
    FScreenMatrix, FProjectionMatrix: TMatrix3D;
    FInvScreenMatrix, FInvProjectionMatrix: TMatrix3D;
    FCenterOffset: TPosition;
    FParent: TWindowHandle;
    FWidth, FHeight: Integer;
    FScale: Single;
    FTexture: TTexture;
    FLights: TLightDescriptionList;
    { style }
    FMultisample: TMultisample;
    FDepthStencil: Boolean;
    { camera }
    FCurrentMatrix: TMatrix3D;
    FCurrentCameraMatrix: TMatrix3D;
    FCurrentCameraInvMatrix: TMatrix3D;
    FCurrentAngleOfView: Single;
    { materials }
    FDefaultMaterial: TMaterial;
    { renderto }
    FRenderToMatrix: TMatrix3D;
    function GetCurrentState(AIndex: TContextState): Boolean;
    function GetProjectionMatrix: TMatrix3D;
    function GetScreenMatrix: TMatrix3D;
    procedure ApplyMaterial(const Material: TMaterial);
    procedure ResetMaterial(const Material: TMaterial);
    function GetCurrentModelViewProjectionMatrix: TMatrix3D;
    procedure DrawPrimitivesMultiBatch(const AKind: TPrimitivesKind; const Vertices, Indices: Pointer;
      const VertexDeclaration: TVertexDeclaration; const VertexSize, VertexCount, IndexSize, IndexCount: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    { buffer }
    procedure DoFreeBuffer; virtual; abstract;
    procedure DoResize; virtual; abstract;
    procedure DoCreateBuffer; virtual; abstract;
    procedure DoCopyToBitmap(const Dest: TBitmap; const ARect: TRect); virtual;
    procedure DoCopyToBits(const Bits: Pointer; const Pitch: Integer; const ARect: TRect); virtual; abstract;
    { rendering }
    /// <summary>Used to initialize Scale property</summary>
    function GetContextScale: Single; virtual;
    function DoBeginScene: Boolean; virtual;
    procedure DoEndScene; virtual;
    procedure DoClear(const ATarget: TClearTargets; const AColor: TAlphaColor; const ADepth: single; const AStencil: Cardinal); virtual; abstract;
    { states }
    procedure DoSetContextState(AState: TContextState); virtual; abstract;
    procedure DoSetStencilOp(const Fail, ZFail, ZPass: TStencilOp); virtual; abstract;
    procedure DoSetStencilFunc(const Func: TStencilfunc; Ref, Mask: Cardinal); virtual; abstract;
    { scissor }
    procedure DoSetScissorRect(const ScissorRect: TRect); virtual; abstract;
    { drawing }
    /// <summary> Provides a mechanism to draw the specified batch of primitives on currently selected hardware-accelerated
    /// layer. This method may support only a limited number of vertices and/or primitives. It may be called multiple
    /// times by DoDrawPrimitives to render larger buffers. </summary>
    procedure DoDrawPrimitivesBatch(const AKind: TPrimitivesKind; const Vertices, Indices: Pointer;
      const VertexDeclaration: TVertexDeclaration; const VertexSize, VertexCount, IndexSize,
      IndexCount: Integer); virtual; abstract;
    /// <summary> This either provides a mechanism to draw the specified primitives (without any limitations) either
    /// directly in hardware or by dividing the buffers in batches and then calling DoDrawPrimitivesBatch to render
    /// each individual batch. </summary>
    procedure DoDrawPrimitives(const AKind: TPrimitivesKind; const Vertices, Indices: Pointer;
      const VertexDeclaration: TVertexDeclaration; const VertexSize, VertexCount, IndexSize,
      IndexCount: Integer); virtual;
    { texture }
    class procedure DoInitializeTexture(const Texture: TTexture); virtual; abstract;
    class procedure DoFinalizeTexture(const Texture: TTexture); virtual; abstract;
    class procedure DoUpdateTexture(const Texture: TTexture; const Bits: Pointer; const Pitch: Integer); virtual; abstract;
    { bitmap }
    class function DoBitmapToTexture(const Bitmap: TBitmap): TTexture; virtual;
    { shaders }
    class procedure DoInitializeShader(const Shader: TContextShader); virtual; abstract;
    class procedure DoFinalizeShader(const Shader: TContextShader); virtual; abstract;
    procedure DoSetShaders(const VertexShader, PixelShader: TContextShader); virtual; abstract;
    procedure DoSetShaderVariable(const Name: string; const Data: array of TVector3D); overload; virtual; abstract;
    procedure DoSetShaderVariable(const Name: string; const Texture: TTexture); overload; virtual; abstract;
    procedure DoSetShaderVariable(const Name: string; const Matrix: TMatrix3D); overload; virtual; abstract;
    { IFreeNotification }
    procedure FreeNotification(AObject: TObject); virtual;
    { constructors }
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AMultisample: TMultisample; const ADepthStencil: Boolean); virtual;
    constructor CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample;
      const ADepthStencil: Boolean); virtual;
    procedure InitContext; virtual;
    /// <summary> Returns supported limit in index buffers on currently selected hardware-accelerated layer. </summary>
    function GetIndexBufferSupport: TIndexBufferSupport; virtual;
  public
    destructor Destroy; override;
    procedure SetSize(const AWidth, AHeight: Integer);
    procedure SetMultisample(const Multisample: TMultisample);
    procedure SetStateFromContext(const AContext: TContext3D);
    class procedure ResetStates; static;
    property BeginSceneCount: Integer read FBeginSceneCount;
    class property GlobalBeginSceneCount: Integer read FGlobalBeginSceneCount;
    { render to }
    procedure SetRenderToMatrix(const Matrix: TMatrix3D);
    { buffer }
    procedure FreeBuffer;
    procedure Resize;
    procedure CreateBuffer;
    procedure CopyToBitmap(const Dest: TBitmap; const ARect: TRect);
    procedure CopyToBits(const Bits: Pointer; const Pitch: Integer; const ARect: TRect);
    { rendering }
    function BeginScene: Boolean;
    procedure EndScene;
    procedure Clear(const AColor: TAlphaColor); overload;
    procedure Clear(const ATarget: TClearTargets; const AColor: TAlphaColor; const ADepth: single; const AStencil: Cardinal); overload;
    { matrix }
    procedure SetMatrix(const M: TMatrix3D);
    procedure SetCameraMatrix(const M: TMatrix3D);
    procedure SetCameraAngleOfView(const Angle: Single);
    { states }
    procedure PushContextStates;
    procedure PopContextStates;
    procedure SetContextState(const State: TContextState);
    procedure SetStencilOp(const Fail, ZFail, ZPass: TStencilOp);
    procedure SetStencilFunc(const Func: TStencilfunc; Ref, Mask: Cardinal);
    procedure SetScissorRect(const ScissorRect: TRect);
    { drawing }
    procedure DrawTriangles(const Vertices: TVertexBuffer; const Indices: TIndexBuffer;
      const Material: TMaterial; const Opacity: Single);
    procedure DrawLines(const Vertices: TVertexBuffer; const Indices: TIndexBuffer; const Material: TMaterial; const Opacity: Single);
    procedure DrawPoints(const Vertices: TVertexBuffer; const Indices: TIndexBuffer; const Material: TMaterial; const Opacity: Single);
    procedure DrawPrimitives(const AKind: TPrimitivesKind; const Vertices, Indices: Pointer;
      const VertexDeclaration: TVertexDeclaration; const VertexSize, VertexCount, IndexSize, IndexCount: Integer;
      const Material: TMaterial; const Opacity: Single);
    procedure FillRect(const TopLeft, BottomRight: TPoint3D; const Opacity: Single; const Color: TAlphaColor);
    procedure FillCube(const Center, Size: TPoint3D; const Opacity: Single; const Color: TAlphaColor);
    procedure DrawLine(const StartPoint, EndPoint: TPoint3D; const Opacity: Single; const Color: TAlphaColor);
    procedure DrawRect(const TopLeft, BottomRight: TPoint3D; const Opacity: Single; const Color: TAlphaColor);
    procedure DrawCube(const Center, Size: TPoint3D; const Opacity: Single; const Color: TAlphaColor);
    procedure FillPolygon(const Center, Size: TPoint3D; const Rect: TRectF; const Points: TPolygon;
      const Material: TMaterial; const Opacity: Single; Front: Boolean = True; Back: Boolean = True;
      Left: Boolean = True);
    { textures }
    class procedure InitializeTexture(const Texture: TTexture);
    class procedure FinalizeTexture(const Texture: TTexture);
    class procedure UpdateTexture(const Texture: TTexture; const Bits: Pointer; const Pitch: Integer);
    { bitmap }
    class function BitmapToTexture(const Bitmap: TBitmap): TTexture;
    { shaders }
    class procedure InitializeShader(const Shader: TContextShader);
    class procedure FinalizeShader(const Shader: TContextShader);
    procedure SetShaders(const VertexShader, PixelShader: TContextShader);
    procedure SetShaderVariable(const Name: string; const Data: array of TVector3D); overload;
    procedure SetShaderVariable(const Name: string; const Texture: TTexture); overload;
    procedure SetShaderVariable(const Name: string; const Matrix: TMatrix3D); overload;
    procedure SetShaderVariable(const Name: string; const Color: TAlphaColor); overload;
    { pick }
    procedure Pick(X, Y: Single; const AProj: TProjection; var RayPos, RayDir: TVector3D);
    function WorldToScreen(const AProj: TProjection; const P: TPoint3D): TPoint3D;
    { states }
    property CurrentModelViewProjectionMatrix: TMatrix3D read GetCurrentModelViewProjectionMatrix;
    property CurrentMatrix: TMatrix3D read FCurrentMatrix;
    property CurrentCameraMatrix: TMatrix3D read FCurrentCameraMatrix;
    property CurrentCameraInvMatrix: TMatrix3D read FCurrentCameraInvMatrix;
    property CurrentProjectionMatrix: TMatrix3D read GetProjectionMatrix;
    property CurrentScreenMatrix: TMatrix3D read GetScreenMatrix;
    property CurrentStates[AIndex: TContextState]: Boolean read GetCurrentState;
    class property CurrentContext: TContext3D read FCurrentContext;
    class property CurrentOpacity: Single read FCurrentOpacity;
    class property CurrentVertexShader: TContextShader read FCurrentVertexShader;
    class property CurrentPixelShader: TContextShader read FCurrentPixelShader;
    class property CurrentScissorRect: TRect read FCurrentScissorRect;
    { lights }
    property Lights: TLightDescriptionList read FLights;
    { caps }
    class function Style: TContextStyles; virtual;
    class function MaxLightCount: Integer; virtual;
    class function MaxTextureSize: Integer; virtual; abstract;
    class function TextureUnitCount: Integer; virtual;
    class function PixelFormat: TPixelFormat; virtual; abstract;
    class function PixelToPixelPolygonOffset: TPointF; virtual;
    { statistic }
    class property FPS: Double read FFPS;
    class property ChangeStateCount: Integer read FChangeStateCount;
    class property ChangeShaderCount: Integer read FChangeStateCount;
    { materials }
    property DefaultMaterial: TMaterial read FDefaultMaterial;
    { properties }
    property CenterOffset: TPosition read FCenterOffset;
    property Height: Integer read FHeight;
    property Width: Integer read FWidth;
    /// <summary>Scale factor of context depends on real resolution of context. It gets from Texture.TextureScale or TWindowHandle.Scale.</summary>
    property Scale: Single read FScale;
    property Texture: TTexture read FTexture;
    property DepthStencil: Boolean read FDepthStencil;
    property Multisample: TMultisample read FMultisample;
    /// <summary> Indicates supported limit in index buffers on currently selected hardware-accelerated layer. </summary>
    property IndexBufferSupport: TIndexBufferSupport read GetIndexBufferSupport;
    { Is this a valid/active context? }
    class function Valid: Boolean; virtual; abstract;
    { Window handle }
    property Parent: TWindowHandle read FParent;
  end;

  TContextClass = class of TContext3D;

  EContextManagerException = class(Exception);

  TContextManager = class sealed
  private type
    TContextClassRec = record
      ContextClass: TContextClass;
      Default: Boolean;
    end;
  strict private
    class var FContextList: TList<TContextClassRec>;
    class var FDefaultContextClass: TContextClass;
  private
    class function GetDefaultContextClass: TContextClass; static;
    class function GetContextCount: Integer; static;
  public
    // Reserved for internal use only - do not call directly!
    class procedure UnInitialize;
    // Register a rendering Canvas class
    class procedure RegisterContext(const ContextClass: TContextClass; const ADefault: Boolean);
    class property ContextCount: Integer read GetContextCount;
    // Return default context class
    class property DefaultContextClass: TContextClass read GetDefaultContextClass;
    // Helper for shaders
    class procedure InitializeShader(const Shader: TContextShader);
    class procedure FinalizeShader(const Shader: TContextShader);
    // Creation
    class function CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AMultisample: TMultisample; const ADepthStencil: Boolean): TContext3D;
    class function CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample;
      const ADepthStencil: Boolean): TContext3D;
  end;

{ TPosition3D }

  TPosition3D = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FY: Single;
    FX: Single;
    FZ: Single;
    FDefaultValue: TPoint3D;
    FOnChangeY: TNotifyEvent;
    FOnChangeX: TNotifyEvent;
    FOnChangeZ: TNotifyEvent;
    procedure SetPoint3D(const Value: TPoint3D);
    procedure SetX(const Value: Single);
    procedure SetY(const Value: Single);
    procedure SetZ(const Value: Single);
    function GetPoint3D: TPoint3D;
    function GetVector: TVector3D;
    procedure SetVector(const Value: TVector3D);
    function IsXStored: Boolean;
    function IsYStored: Boolean;
    function IsZStored: Boolean;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadPoint(Reader: TReader);
    procedure WritePoint(Writer: TWriter);
  public
    constructor Create(const ADefaultValue: TPoint3D); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure SetPoint3DNoChange(const P: TPoint3D);
    procedure SetVectorNoChange(const P: TVector3D);
    function Empty: Boolean;
    property Point: TPoint3D read GetPoint3D write SetPoint3D;
    property Vector: TVector3D read GetVector write SetVector;
    property DefaultValue: TPoint3D read FDefaultValue write FDefaultValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeX: TNotifyEvent read FOnChangeX write FOnChangeX;
    property OnChangeY: TNotifyEvent read FOnChangeY write FOnChangeY;
    property OnChangeZ: TNotifyEvent read FOnChangeZ write FOnChangeZ;
  published
    property X: Single read FX write SetX stored IsXStored nodefault;
    property Y: Single read FY write SetY stored IsYStored nodefault;
    property Z: Single read FZ write SetZ stored IsZStored nodefault;
  end;

{ Utils }
function VertexSize(const AFormat: TVertexFormats): Integer;
function GetVertexOffset(const APosition: TVertexFormat; const AFormat: TVertexFormats): Integer;

{ Intersection }

function RayCastPlaneIntersect(const RayPos, RayDir, PlanePoint, PlaneNormal: TPoint3D;
  var Intersection: TPoint3D): Boolean;

function RayCastSphereIntersect(const RayPos, RayDir, SphereCenter: TPoint3D; const SphereRadius: Single;
  var IntersectionNear, IntersectionFar: TPoint3D): Integer;

function RayCastEllipsoidIntersect(const RayPos, RayDir, EllipsoidCenter: TPoint3D; const XRadius, YRadius,
  ZRadius: Single; var IntersectionNear, IntersectionFar: TPoint3D): Integer;

function RayCastCuboidIntersect(const RayPos, RayDir, CuboidCenter: TPoint3D; const Width, Height, Depth: Single;
  var IntersectionNear, IntersectionFar: TPoint3D): Integer;

function RayCastTriangleIntersect(const RayPos, RayDir: TPoint3D; const Vertex1, Vertex2, Vertex3: TPoint3D;
  var Intersection: TPoint3D): Boolean;

function WideGetToken(var Pos: Integer; const S: string; const Separators: string; const Stop: string = ''): string;

implementation

uses
  System.Math, System.TypInfo, System.RTLConsts, FMX.Platform, FMX.Materials, FMX.Surfaces, FMX.Consts, FMX.Utils;

type
  TOpenObject = class(TFmxObject);

{ Utils }

const

  DefaultMaterialColor = claRed;

  // to be used as descriptive indices
  X = 0;
  Y = 1;
  Z = 2;
  W = 3;

  cZero: Single = 0.0;
  cOne: Single = 1.0;
  cOneDotFive: Single = 0.5;

function WideGetToken(var Pos: Integer; const S: string; const Separators: string;
  const Stop: string = ''): string;
var
  len: Integer;
begin
  Result := '';
  len := S.Length;
  { skip first separators }
  while Pos < len do
  begin
    if not Separators.Contains(S.Chars[Pos]) then
      Break;
    Inc(Pos);
  end;
  { get }
  while Pos < len do
  begin
    if Stop.Contains(S.Chars[Pos]) then
      Break;
    if Separators.Contains(S.Chars[Pos]) then
      Break;
    Result := Result + S.Chars[Pos];
    Inc(Pos);
  end;
  { skip separators }
  while Pos < len do
  begin
    if not Separators.Contains(S.Chars[Pos]) then
      Break;
    Inc(Pos);
  end;
end;

{ Vertices }

function VertexSize(const AFormat: TVertexFormats): Integer;
begin
  Result := 0;
  if TVertexFormat.Vertex in AFormat then
    Result := Result + SizeOf(Single) * 3;
  if TVertexFormat.Normal in AFormat then
    Result := Result + SizeOf(Single) * 3;
  if TVertexFormat.Color0 in AFormat then
    Result := Result + SizeOf(Cardinal);
  if TVertexFormat.Color1 in AFormat then
    Result := Result + SizeOf(Cardinal);
  if TVertexFormat.Color2 in AFormat then
    Result := Result + SizeOf(Cardinal);
  if TVertexFormat.Color3 in AFormat then
    Result := Result + SizeOf(Cardinal);
  if TVertexFormat.TexCoord0 in AFormat then
    Result := Result + SizeOf(Single) * 2;
  if TVertexFormat.TexCoord1 in AFormat then
    Result := Result + SizeOf(Single) * 2;
  if TVertexFormat.TexCoord2 in AFormat then
    Result := Result + SizeOf(Single) * 2;
  if TVertexFormat.TexCoord3 in AFormat then
    Result := Result + SizeOf(Single) * 2;
  if TVertexFormat.BiNormal in AFormat then
    Result := Result + SizeOf(Single) * 3;
  if TVertexFormat.Tangent in AFormat then
    Result := Result + SizeOf(Single) * 3;
  if TVertexFormat.ColorF0 in AFormat then
    Result := Result + SizeOf(Single) * 4;
  if TVertexFormat.ColorF1 in AFormat then
    Result := Result + SizeOf(Single) * 4;
  if TVertexFormat.ColorF2 in AFormat then
    Result := Result + SizeOf(Single) * 4;
  if TVertexFormat.ColorF3 in AFormat then
    Result := Result + SizeOf(Single) * 4;
end;

function GetVertexOffset(const APosition: TVertexFormat; const AFormat: TVertexFormats): Integer;
begin
  Result := 0;
  if APosition = TVertexFormat.Vertex then Exit;
  if TVertexFormat.Vertex in AFormat then
    Result := Result + SizeOf(Single) * 3;
  if APosition = TVertexFormat.Normal then Exit;
  if TVertexFormat.Normal in AFormat then
    Result := Result + SizeOf(Single) * 3;
  if APosition = TVertexFormat.Color0 then Exit;
  if TVertexFormat.Color0 in AFormat then
    Result := Result + SizeOf(Cardinal);
  if APosition = TVertexFormat.Color1 then Exit;
  if TVertexFormat.Color1 in AFormat then
    Result := Result + SizeOf(Cardinal);
  if APosition = TVertexFormat.Color2 then Exit;
  if TVertexFormat.Color2 in AFormat then
    Result := Result + SizeOf(Cardinal);
  if APosition = TVertexFormat.Color3 then Exit;
  if TVertexFormat.Color3 in AFormat then
    Result := Result + SizeOf(Cardinal);
  if APosition = TVertexFormat.TexCoord0 then Exit;
  if TVertexFormat.TexCoord0 in AFormat then
    Result := Result + SizeOf(Single) * 2;
  if APosition = TVertexFormat.TexCoord1 then Exit;
  if TVertexFormat.TexCoord1 in AFormat then
    Result := Result + SizeOf(Single) * 2;
  if APosition = TVertexFormat.TexCoord2 then Exit;
  if TVertexFormat.TexCoord2 in AFormat then
    Result := Result + SizeOf(Single) * 2;
  if APosition = TVertexFormat.TexCoord3 then Exit;
  if TVertexFormat.TexCoord3 in AFormat then
    Result := Result + SizeOf(Single) * 2;
  if APosition = TVertexFormat.BiNormal then Exit;
  if TVertexFormat.BiNormal in AFormat then
    Result := Result + SizeOf(Single) * 3;
  if APosition = TVertexFormat.Tangent then Exit;
  if TVertexFormat.Tangent in AFormat then
    Result := Result + SizeOf(Single) * 3;
  if APosition = TVertexFormat.ColorF0 then Exit;
  if TVertexFormat.ColorF0 in AFormat then
    Result := Result + SizeOf(Single) * 4;
  if APosition = TVertexFormat.ColorF1 then Exit;
  if TVertexFormat.ColorF1 in AFormat then
    Result := Result + SizeOf(Single) * 4;
  if APosition = TVertexFormat.ColorF2 then Exit;
  if TVertexFormat.ColorF2 in AFormat then
    Result := Result + SizeOf(Single) * 4;
  if APosition = TVertexFormat.ColorF3 then Exit;
  if TVertexFormat.ColorF3 in AFormat then
    Result := Result + SizeOf(Single) * 4;
end;

{$EXCESSPRECISION OFF}

function IsEssentiallyZero(const Value: Single): Boolean;
begin
  Result := ((Value < Epsilon2) and (Value > -Epsilon2));
end;

function IsNotEssentiallyZero(const Value: Single): Boolean;
begin
  Result := ((Value > Epsilon2) or (Value < -Epsilon2));
end;

{
  See: http://en.wikipedia.org/wiki/Line-sphere_intersection
  See: http://www.ccs.neu.edu/home/fell/CSU540/programs/RayTracingFormulas.htm
}
function RayCastSphereIntersect(const RayPos, RayDir, SphereCenter: TPoint3D; const SphereRadius: Single;
  var IntersectionNear, IntersectionFar: TPoint3D): Integer;
var
  A, B, C, B2, FourAC, Discriminant, LRoot, TwoA, LFactor: Single;
  LTempVec: TPoint3D;
begin
  A := RayDir.X * RayDir.X + RayDir.Y * RayDir.Y + RayDir.Z * RayDir.Z;
  B := 2 * (RayDir.X * (RayPos.X - SphereCenter.X) + RayDir.Y * (RayPos.Y - SphereCenter.Y) + RayDir.Z * (RayPos.Z -
    SphereCenter.Z));
  C := SphereCenter.X * SphereCenter.X + SphereCenter.Y * SphereCenter.Y + SphereCenter.Z * SphereCenter.Z + RayPos.X *
    RayPos.X + RayPos.Y * RayPos.Y + RayPos.Z * RayPos.Z - 2 * (SphereCenter.X * RayPos.X + SphereCenter.Y * RayPos.Y +
    SphereCenter.Z * RayPos.Z) - SphereRadius * SphereRadius;

  B2 := B * B;
  FourAC := 4 * A * C;
  Discriminant := B2 - FourAC;

  if Discriminant < 0 then
    Result := 0
  else if Discriminant = 0 then
  begin
    Result := 1;
    LFactor := -B / (2 * A); // we already know the descriminant is 0
    IntersectionNear := RayPos + (RayDir * LFactor);
    IntersectionFar := IntersectionNear;
  end
  else
  begin
    Result := 2;
    LRoot := Sqrt(B2 - FourAC);
    TwoA := 2 * A;
    LFactor := (-B - LRoot) / TwoA;
    IntersectionNear := RayPos + (RayDir * LFactor);
    LFactor := (-B + LRoot) / TwoA;
    IntersectionFar := RayPos + (RayDir * LFactor);
    if RayPos.Distance(IntersectionNear) > RayPos.Distance(IntersectionFar) then
    begin
      LTempVec := IntersectionNear;
      IntersectionNear := IntersectionFar;
      IntersectionFar := LTempVec;
    end;
  end;
end;

{
  We can use the Sphere Intersect algorithm if we distort space so we have a single common radius
}
function RayCastEllipsoidIntersect(const RayPos, RayDir, EllipsoidCenter: TPoint3D; const XRadius, YRadius,
  ZRadius: Single; var IntersectionNear, IntersectionFar: TPoint3D): Integer;
var
  LCommonRadius, LFactorX, LFactorY, LFactorZ: Single;
  LRayPos, LRayDir, LSphereCenter: TPoint3D;
begin
  // avoid degenerate cases (where ellipsoid is a plane or line)
  if IsNotEssentiallyZero(XRadius) and IsNotEssentiallyZero(YRadius) and IsNotEssentiallyZero(ZRadius) then
  begin
    LCommonRadius := XRadius;
    LCommonRadius := Max(LCommonRadius, YRadius);
    LCommonRadius := Max(LCommonRadius, ZRadius);
    LFactorX := LCommonRadius / XRadius;
    LFactorY := LCommonRadius / YRadius;
    LFactorZ := LCommonRadius / ZRadius;
    LRayPos := TPoint3D.Create(RayPos.X * LFactorX, RayPos.Y * LFactorY, RayPos.Z * LFactorZ);
    LRayDir := TPoint3D.Create(RayDir.X * LFactorX, RayDir.Y * LFactorY, RayDir.Z * LFactorZ);
    LSphereCenter := TPoint3D.Create(EllipsoidCenter.X * LFactorX, EllipsoidCenter.Y * LFactorY, EllipsoidCenter.Z *
      LFactorZ);
    Result := RayCastSphereIntersect(LRayPos, LRayDir, LSphereCenter, LCommonRadius, IntersectionNear,
      IntersectionFar);
    // adjust intersection points as needed
    if Result > 0 then
    begin
      IntersectionNear.X := IntersectionNear.X / LFactorX;
      IntersectionNear.Y := IntersectionNear.Y / LFactorY;
      IntersectionNear.Z := IntersectionNear.Z / LFactorZ;
      IntersectionFar.X := IntersectionFar.X / LFactorX;
      IntersectionFar.Y := IntersectionFar.Y / LFactorY;
      IntersectionFar.Z := IntersectionFar.Z / LFactorZ;
    end;
  end
  else
    Result := 0;
end;

function RayCastCuboidIntersect(const RayPos, RayDir, CuboidCenter: TPoint3D; const Width, Height, Depth: Single;
  var IntersectionNear, IntersectionFar: TPoint3D): Integer;
var
  LWidth, LHeight, LDepth: Single;
  LContinueSearch: Boolean;
  A, B, C: Single;
  LIntercepts: array of TPoint3D;
  LDimensionVec, LThicknessVec: TPoint3D;
  I: Integer;

const
  Root3Over2: Single = 0.866025404;

  function TryEllipsoidShortCut(const W, H, D: Single): Boolean;
  var
    LMax, LMin: Single;
  begin
    LMin := W;
    LMin := Min(LMin, H);
    LMin := Min(LMin, D);
    LMax := W;
    LMax := Max(LMax, H);
    LMax := Max(LMax, D);
    Result := (LMin / LMax) > 0.1;
  end;

  function Inside(const Value: TPoint3D): Boolean;
  begin
    Result := (Abs(Value.X - CuboidCenter.X) <= (0.501 * LWidth)) and (Abs(Value.Y - CuboidCenter.Y) <= (0.501 *
      LHeight)) and (Abs(Value.Z - CuboidCenter.Z) <= (0.501 * LDepth));
  end;

  // FireMonkey layers (which are basically 2D) have a hard coded thickness of 0.01
  function IsThickerThan2DLayer(const Value: Single): Boolean;
  begin
    Result := (Value > 0.01) or (Value < -0.01);
  end;

begin
  Result := 0;
  LWidth := Abs(Width);
  LHeight := Abs(Height);
  LDepth := Abs(Depth);

  // try to check as plane
  if IsEssentiallyZero(LDepth) and IsNotEssentiallyZero(LWidth) and IsNotEssentiallyZero(LHeight) then
  begin
    if RayCastPlaneIntersect(RayPos, RayDir, CuboidCenter, TPoint3D.Create(0, 0, 1), IntersectionNear) and
      (Abs(IntersectionNear.X) < LWidth / 2) and (Abs(IntersectionNear.Y) < Height / 2) then
    begin
      Result := 1;
      IntersectionFar := IntersectionNear;
    end;
    Exit;
  end;

  if IsNotEssentiallyZero(LDepth) and IsEssentiallyZero(LWidth) and IsNotEssentiallyZero(LHeight) then
  begin
    if RayCastPlaneIntersect(RayPos, RayDir, CuboidCenter, TPoint3D.Create(1, 0, 0), IntersectionNear) and
      (Abs(IntersectionNear.Z) < LDepth / 2) and (Abs(IntersectionNear.Y) < Height / 2) then
    begin
      Result := 1;
      IntersectionFar := IntersectionNear;
    end;
    Exit;
  end;

  if IsNotEssentiallyZero(LDepth) and IsNotEssentiallyZero(LWidth) and IsEssentiallyZero(LHeight) then
  begin
    if RayCastPlaneIntersect(RayPos, RayDir, CuboidCenter, TPoint3D.Create(0, 1, 0), IntersectionNear) and
       (Abs(IntersectionNear.X) < LWidth / 2) and (Abs(IntersectionNear.Z) < Depth / 2) then
    begin
      Result := 1;
      IntersectionFar := IntersectionNear;
    end;
    Exit;
  end;

  // is empty
  if IsEssentiallyZero(LDepth) and IsEssentiallyZero(LWidth) and IsEssentiallyZero(LHeight) then
  begin
    Result := 0;
    Exit;
  end;

  SetLength(LIntercepts, 2);
  // To find the real answer, we need to see how the ray intersects with the faces of the cuboid.
  // As a shortcut, we can see if there is intersection with an ellipsoid that encompasses the
  // entirety of the cuboid. Don't bother if the aspect ratio is too large.
  if TryEllipsoidShortCut(LWidth, LHeight, LDepth) then
  begin
    // Derivation:
    //
    // Equation of ellipsoid (http://en.wikipedia.org/wiki/Ellipsoid):
    //
    // (x^2)/(a^2) + (y^2)/(b^2) + (z^2)/(c^2) = 1
    //
    // We also know that for the ellipsoid inscribed INSIDE the cuboid:
    //
    //  a' = Width/2
    //  b' = Height/2
    //  c' = Depth/2
    //
    // To find the ellipsoid which encloses the cuboid, we need to simply scale
    // up the ellipsoid which is inscribed within. Thus:
    //
    //  a = factor * a' = factor * Width/2
    //  b = factor * b' = factor * Height/2
    //  c = factor * c' = factor * Depth/2
    //
    // We know one solution for the equation of the ellipsoid which encloses the
    // cuboid is found when:
    //
    // x = Width/2
    // y = Height/2
    // z = Depth/2
    //
    // thus:
    //
    // ((Width/2)^2)/(a^2) + ((Height/2)^2)/(b^2)) + ((Depth/2)^2)/(c^2) = 1
    //
    // substitute a, b, c and simplify:
    //
    // 1/factor^2 + 1/factor^2 + 1/factor^2 = 1
    //
    // 3/factor^2 = 1
    //
    // factor = SquareRoot(3)
    //
    // yielding:
    //
    //  a = SquareRoot(3) * Width/2
    //  b = SquareRoot(3) * Height/2
    //  c = SquareRoot(3) * Depth/2

    A := Root3Over2 * LWidth;
    B := Root3Over2 * LHeight;
    C := Root3Over2 * LDepth;

    LContinueSearch := RayCastEllipsoidIntersect(RayPos, RayDir, CuboidCenter, A, B, C, LIntercepts[0],
      LIntercepts[1]) > 0;
  end
  else
    LContinueSearch := True;

  if LContinueSearch then
  begin
    // We failed the ellipsoid check, now we need to do the hard work and check each face
    Result := 0;

    // store these in a vector so we can iterate over them
    LDimensionVec := TPoint3D.Create(LWidth / 2, LHeight / 2, LDepth / 2);
    LThicknessVec := TPoint3D.Create(Min(LHeight, LDepth), Min(LWidth, LDepth), Min(LWidth, LHeight));

    for I := 0 to 2 do
      if (Result < 2) and IsNotEssentiallyZero(RayDir.V[I]) and IsThickerThan2DLayer(LThicknessVec.V[I]) then
      begin
        LIntercepts[Result] := RayPos + (RayDir * ((CuboidCenter.V[I] - LDimensionVec.V[I] - RayPos.V[I]) /
          RayDir.V[I]));
        if Inside(LIntercepts[Result]) then
          Inc(Result);

        if Result < 2 then
        begin
          LIntercepts[Result] := RayPos + (RayDir * ((CuboidCenter.V[I] + LDimensionVec.V[I] - RayPos.V[I]) /
            RayDir.V[I]));
          if Inside(LIntercepts[Result]) then
            Inc(Result);
        end;
      end;

    if Result = 1 then
    begin
      IntersectionNear := LIntercepts[0];
      IntersectionFar := LIntercepts[0];
    end
    else if Result = 2 then
    begin
      if RayPos.Distance(LIntercepts[0]) < RayPos.Distance(LIntercepts[1]) then
      begin
        IntersectionNear := LIntercepts[0];
        IntersectionFar := LIntercepts[1];
      end
      else
      begin
        IntersectionNear := LIntercepts[1];
        IntersectionFar := LIntercepts[0];
      end;
    end;
  end
  else
    Result := 0;
end;

{
  See: http://en.wikipedia.org/wiki/Line-plane_intersection
  See: http://paulbourke.net/geometry/planeline/
}
function RayCastPlaneIntersect(const RayPos, RayDir, PlanePoint, PlaneNormal: TPoint3D;
  var Intersection: TPoint3D): Boolean;
var
  LDotProd, LFactor: Single;
begin
  // Is the Ray parallel to the plane?
  LDotProd := RayDir.DotProduct(PlaneNormal);
  if IsNotEssentiallyZero(LDotProd) then
  begin
    LFactor := (PlanePoint - RayPos).DotProduct(PlaneNormal) / LDotProd;
    if LFactor > 0 then
    begin
      Result := True;
      Intersection := RayPos + (RayDir * LFactor);
    end
    else
      Result := False; // The Ray points away from the plane
  end
  else
    Result := False;
end;

{
  See: http://en.wikipedia.org/wiki/Barycentric_coordinate_system_(mathematics)#Determining_if_a_point_is_inside_a_triangle
  See: http://mathworld.wolfram.com/BarycentricCoordinates.html
  See: http://www.blackpawn.com/texts/pointinpoly/default.html
}

function SameSide(const P1, P2, A, B: TPoint3D): Boolean;
var
  CP1, CP2: TPoint3D;
begin
  CP1 := (B - A).CrossProduct(P1 - A);
  CP2 := (B - A).CrossProduct(P2 - A);
  if CP1.DotProduct(CP2) >= 0 then
    Result := True
  else
    Result := False;
end;

function RayCastTriangleIntersect(const RayPos, RayDir, Vertex1, Vertex2, Vertex3: TPoint3D;
  var Intersection: TPoint3D): Boolean;
var
  Normal, Point: TPoint3D;
begin
  Normal := (Vertex1 - Vertex2).CrossProduct(Vertex3 - Vertex1);

  if RayCastPlaneIntersect(RayPos, RayDir, Vertex1, Normal, Point) then
    Result := SameSide(Point, Vertex1, Vertex2, Vertex3) and SameSide(Point, Vertex2, Vertex3, Vertex1) and
      SameSide(Point, Vertex3, Vertex1, Vertex2)
  else
    Result := False;
end;

{$EXCESSPRECISION ON}

{ TMeshData }

constructor TMeshData.Create;
begin
  inherited;
  BoundingBoxNeedsUpdate;
  FVertexBuffer := TVertexBuffer.Create([TVertexFormat.Vertex, TVertexFormat.Normal, TVertexFormat.TexCoord0, TVertexFormat.BiNormal, TVertexFormat.Tangent], 0);
  FIndexBuffer := TIndexBuffer.Create(0, TIndexFormat.UInt32);
end;

destructor TMeshData.Destroy;
begin
  FreeAndNil(FVertexBuffer);
  FreeAndNil(FIndexBuffer);
  inherited;
end;

procedure TMeshData.Assign(Source: TPersistent);
begin
  if Source is TMeshData then
  begin
    BoundingBoxNeedsUpdate;
    FVertexBuffer.Assign(TMeshData(Source).FVertexBuffer);
    FIndexBuffer.Assign(TMeshData(Source).FIndexBuffer);
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end
  else
    inherited
end;

procedure TMeshData.AssignFromMeshVertex(const Vertices: array of TMeshVertex; const Indices: array of Cardinal);
var
  I: Integer;
begin
  BoundingBoxNeedsUpdate;
  FVertexBuffer.Length := Length(Vertices);
  for I := 0 to FVertexBuffer.Length - 1 do
  begin
    FVertexBuffer.Vertices[I] := TPoint3D.Create(Vertices[I].x, Vertices[I].y, Vertices[I].z);
    FVertexBuffer.Normals[I] := TPoint3D.Create(Vertices[I].nx, Vertices[I].ny, Vertices[I].nz);
    FVertexBuffer.TexCoord0[I] := TPointF.Create(Vertices[I].tu, Vertices[I].tv);
  end;
  FIndexBuffer.Length := Length(Indices);
  Move(Indices[0], FIndexBuffer.Buffer^, FIndexBuffer.Size);
end;

procedure TMeshData.AssignFromMeshVertex(const Vertices: array of TMeshVertex;
  const Indices: array of Word);
var
  I: Integer;
begin
  BoundingBoxNeedsUpdate;
  FVertexBuffer.Length := Length(Vertices);
  for I := 0 to FVertexBuffer.Length - 1 do
  begin
    FVertexBuffer.Vertices[I] := TPoint3D.Create(Vertices[I].x, Vertices[I].y, Vertices[I].z);
    FVertexBuffer.Normals[I] := TPoint3D.Create(Vertices[I].nx, Vertices[I].ny, Vertices[I].nz);
    FVertexBuffer.TexCoord0[I] := TPointF.Create(Vertices[I].tu, Vertices[I].tv);
  end;
  FIndexBuffer.Length := Length(Indices);
  for I := 0 to FIndexBuffer.Length - 1 do
    FIndexBuffer[I] := Indices[I];
end;

procedure TMeshData.ChangeFormat(const ANewFormat: TVertexFormats);
begin
  BoundingBoxNeedsUpdate;
  FreeAndNil(FVertexBuffer);
  FVertexBuffer := TVertexBuffer.Create(ANewFormat, 0);
end;

procedure TMeshData.CalcFaceNormals(const PropagateFaceNormalsToVertices: Boolean = True);
var
  Index, FaceIndex, Vtx1, Vtx2, Vtx3: Integer;
  Term1, Term2, LNormal: TPoint3D;
begin
  SetLength(FFaceNormals, FIndexBuffer.Length div 3);
  Index := 0;
  FaceIndex := 0;
  while (Index < FIndexBuffer.Length - 2) do
  begin
    Vtx1 := FIndexBuffer[Index];
    Vtx2 := FIndexBuffer[Index + 2];
    Vtx3 := FIndexBuffer[Index + 1];

    Term1 := FVertexBuffer.Vertices[Vtx3] - FVertexBuffer.Vertices[Vtx1];
    Term2 := FVertexBuffer.Vertices[Vtx3] - FVertexBuffer.Vertices[Vtx2];

    LNormal := (Term1.CrossProduct(Term2)).Normalize;
    FFaceNormals[FaceIndex] := LNormal;

    if PropagateFaceNormalsToVertices then
    begin
      FVertexBuffer.Normals[Vtx1] := LNormal;
      FVertexBuffer.Normals[Vtx2] := LNormal;
      FVertexBuffer.Normals[Vtx3] := LNormal;
    end;

    Inc(Index, 3);
    Inc(FaceIndex);
  end;
end;


procedure TMeshData.CalcSmoothNormals(const Method: TCalculateNormalMethod; const WeldEpsilon: Single);
  // This is the fastest method, it solves the normal vector by the indices of each index in a face.
  // In other words, the indices of a face determines the normal of each pointed vertex.
  procedure ResetVertexNormals;
  var
    VertexIndex: Integer;
  begin
    for VertexIndex := 0 to FVertexBuffer.Length - 1 do
      FVertexBuffer.Normals[VertexIndex] := NullPoint3D;
  end;

  // Vertex normalization pass
  procedure NormalizeVertexNormals;
  var
    VertexIndex: Integer;
    LNormal: PPoint3D;
  begin
    for VertexIndex := 0 to FVertexBuffer.Length - 1 do
    begin
      LNormal := FVertexBuffer.GetNormalsPtr(VertexIndex);
      LNormal^ := LNormal.Normalize;
    end;
  end;

var
  Index, FaceIndex, VertexIndex, SubIndex, I: Integer;
  AccNormal, AccVertex, LFaceNormal, LVertexNormal: TPoint3D;
  LNormal: PPoint3D;
  VertexNormalDictionary: TDictionary<TVertexSmoothNormalInfo, TPoint3D>;
  Info: TVertexSmoothNormalInfo;
  InfoPair: TPair<TVertexSmoothNormalInfo, TPoint3D>;
  NormalFactor: Single;
begin
  CalcFaceNormals(False);

  case Method of
    TCalculateNormalMethod.Default:
    begin
      // This method tries to calculate the normal vector by the neighbours faces in the space.
      // For backward compatibility, since the moment this was the first method implemented, is the default.
      for Index := 0 to FVertexBuffer.Length - 1 do
      begin
        AccNormal := NullPoint3D;
        AccVertex := FVertexBuffer.Vertices[Index];

        for FaceIndex := 0 to High(FFaceNormals) do
          for SubIndex := 0 to 2 do
          begin
            VertexIndex := FIndexBuffer[(FaceIndex * 3) + SubIndex];
            if (FVertexBuffer.Vertices[VertexIndex]- AccVertex).Length <= WeldEpsilon then
              AccNormal:= AccNormal + FFaceNormals[FaceIndex];
          end;

        FVertexBuffer.Normals[Index] := AccNormal.Normalize;
      end;
    end;
    TCalculateNormalMethod.Fastest:
    begin
      ResetVertexNormals;
      for FaceIndex := 0 to Length(FFaceNormals) - 1 do
      begin
        LFaceNormal := FFaceNormals[FaceIndex];
        for I := 0 to 2 do
        begin
          SubIndex := (FaceIndex * 3) + I;
          VertexIndex := FIndexBuffer[SubIndex];
          LNormal := FVertexBuffer.GetNormalsPtr(VertexIndex);
          LNormal^ := LNormal^ + LFaceNormal;
        end;
      end;
      NormalizeVertexNormals;
    end;
    else // Method = TCalculateNormalMethod.Precise
    begin
      // This is the slower method. It can be used, by example, to emulate quads to generate the normals of the vertices.
      // A dictionary is used to group similar face normals which are intended to modify the normal vector of the vertex.
      // If the dictionary raise an error because of the amount of memory used, the user can catch the exception and try
      // to generate the normals with different parameters
      VertexNormalDictionary := TDictionary<TVertexSmoothNormalInfo, TPoint3D>.Create;
      try
        ResetVertexNormals;
        if WeldEpsilon > 0.0 then
          NormalFactor := 1.0 / WeldEpsilon
        else
          NormalFactor := 10000.0;
        for FaceIndex := 0 to Length(FFaceNormals) - 1 do
        begin
          LFaceNormal := FFaceNormals[FaceIndex];
          Info.ScaledRoundedX := Round(LFaceNormal.X * NormalFactor);
          Info.ScaledRoundedY := Round(LFaceNormal.Y * NormalFactor);
          Info.ScaledRoundedZ := Round(LFaceNormal.Z * NormalFactor);
          for I := 0 to 2 do
          begin
            SubIndex := (FaceIndex * 3) + I;
            VertexIndex := FIndexBuffer[SubIndex];
            Info.VertexId := VertexIndex;

            if VertexNormalDictionary.TryGetValue(Info, LVertexNormal) then
            begin
              LVertexNormal := LVertexNormal + LFaceNormal;
              VertexNormalDictionary.AddOrSetValue(Info, LVertexNormal);
            end
            else
            begin
              VertexNormalDictionary.Add(Info, LFaceNormal);
            end;
          end;
        end;
        for InfoPair in VertexNormalDictionary do
        begin
          VertexIndex := InfoPair.Key.VertexId;
          LNormal := FVertexBuffer.GetNormalsPtr(VertexIndex);
          LNormal^ := LNormal^ + InfoPair.Value.Normalize;
        end;
        NormalizeVertexNormals;
      finally
        VertexNormalDictionary.Free;
      end;
    end;
  end;
end;

procedure TMeshData.CalcTangentBinormals;
const
  TexCoordTangentEpsilon = 0.0001;
var
  Index, FaceIndex, VtxIndex1, VtxIndex2, VtxIndex3: Integer;
  p0, p1, p2, p, q: TPoint3D;
  uv0, uv1, uv2: TPointF;
  Normal, Tangent, Binormal: TPoint3D;
  s1, t1, s2, t2, coef: Single;
  FaceTangents, FaceBinormals: TPoint3DDynArray;
begin
  CalcFaceNormals;

  SetLength(FaceTangents, Length(FFaceNormals));
  SetLength(FaceBinormals, Length(FFaceNormals));

  for FaceIndex := 0 to High(FFaceNormals) do
  begin
    Normal := FFaceNormals[FaceIndex div 3];

    VtxIndex1 := FIndexBuffer[FaceIndex * 3];
    VtxIndex2 := FIndexBuffer[(FaceIndex * 3) + 1];
    VtxIndex3 := FIndexBuffer[(FaceIndex * 3) + 2];

    p0 := FVertexBuffer.Vertices[VtxIndex1];
    p1 := FVertexBuffer.Vertices[VtxIndex2];
    p2 := FVertexBuffer.Vertices[VtxIndex3];
    p.x := p1.x - p0.x;
    p.y := p1.y - p0.y;
    p.z := p1.z - p0.z;
    q.x := p2.x - p0.x;
    q.y := p2.y - p0.y;
    q.z := p2.z - p0.z;

    uv0 := FVertexBuffer.TexCoord0[VtxIndex1];
    uv1 := FVertexBuffer.TexCoord0[VtxIndex2];
    uv2 := FVertexBuffer.TexCoord0[VtxIndex3];

    s1 := uv1.x - uv0.x;
    t1 := uv1.y - uv0.y;
    s2 := uv2.x - uv0.x;
    t2 := uv2.y - uv0.y;

    if (Abs(s1 * t2 - s2 * t1) > TexCoordTangentEpsilon) then
      Coef := 1.0 / (s1 * t2 - s2 * t1)
    else
      Coef := 1.0;

    Tangent.x := (t2 * p.x - t1 * q.x) * Coef;
    Tangent.y := (t2 * p.y - t1 * q.x) * Coef;
    Tangent.z := (t2 * p.z - t1 * q.z) * Coef;
    Binormal := Normal.CrossProduct(Tangent);

    FaceTangents[FaceIndex] := Tangent.Normalize;
    FaceBinormals[FaceIndex] := Binormal.Normalize;
  end;
  for Index := 0 to FVertexBuffer.Length - 1 do
  begin
    FVertexBuffer.Tangents[Index] := FaceTangents[Index div 3];
    FVertexBuffer.BiNormals[Index] := FaceBiNormals[Index div 3];
  end;
end;

procedure TMeshData.Clear;
begin
  FVertexBuffer.Length := 0;
  FIndexBuffer.Length := 0;
  BoundingBoxNeedsUpdate;
end;

procedure TMeshData.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Mesh', ReadMesh, WriteMesh, FVertexBuffer.Size > 0);
end;

function TMeshData.RayCastIntersect(const Width, Height, Depth: Single; const RayPos, RayDir: TPoint3D;
  var Intersection: TPoint3D): Boolean;
var
  INear, IFar, P1, P2, P3, P: TPoint3D;
  I: Integer;
begin
  // Start with a simple test of the bounding cuboid
  Result := RayCastCuboidIntersect(RayPos, RayDir, NullPoint3D, Width, Height, Depth, INear, IFar) > 0;
  if Result then
  begin
    // Now, reset the result and check the triangles
    Result := False;
    if (VertexBuffer.Size > 0) and (IndexBuffer.Size > 0) then
    begin
      for I := 0 to (IndexBuffer.Length div 3) - 1 do
      begin
        if (IndexBuffer[(I * 3) + 0] < VertexBuffer.Length) and (IndexBuffer[(I * 3) + 1] < VertexBuffer.Length) and
          (IndexBuffer[(I * 3) + 2] < VertexBuffer.Length) then
        begin
          P := VertexBuffer.Vertices[IndexBuffer[(I * 3) + 0]];
          P1 := TPoint3D.Create(P.X * Width, P.Y * Height, P.Z * Depth);
          P := VertexBuffer.Vertices[IndexBuffer[(I * 3) + 1]];
          P2 := TPoint3D.Create(P.X * Width, P.Y * Height, P.Z * Depth);
          P := VertexBuffer.Vertices[IndexBuffer[(I * 3) + 2]];
          P3 := TPoint3D.Create(P.X * Width, P.Y * Height, P.Z * Depth);
          if RayCastTriangleIntersect(RayPos, RayDir, P1, P2, P3, INear) then
          begin
            Intersection := INear;
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

function TMeshData.RayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean;
var
  LNear, LFar, P1, P2, P3: TPoint3D;
  I: Integer;
  LBoundingBox: TBoundingBox;
begin
  // Start with a simple test of the bounding cuboid
  LBoundingBox := GetBoundingBox;
  Result := RayCastCuboidIntersect(RayPos, RayDir, LBoundingBox.GetCenterPoint, LBoundingBox.Width, LBoundingBox.Height,
    LBoundingBox.Depth, LNear, LFar) > 0;
  if Result then
  begin
    // Now, reset the result and check the triangles
    Result := False;
    if (VertexBuffer.Size > 0) and (IndexBuffer.Size > 0) then
      for I := 0 to (IndexBuffer.Length div 3) - 1 do
      begin
        if (IndexBuffer[(I * 3) + 0] < VertexBuffer.Length) and (IndexBuffer[(I * 3) + 1] < VertexBuffer.Length) and
          (IndexBuffer[(I * 3) + 2] < VertexBuffer.Length) then
        begin
          P1 := VertexBuffer.Vertices[IndexBuffer[(I * 3) + 0]];
          P2 := VertexBuffer.Vertices[IndexBuffer[(I * 3) + 1]];
          P3 := VertexBuffer.Vertices[IndexBuffer[(I * 3) + 2]];
          if RayCastTriangleIntersect(RayPos, RayDir, P1, P2, P3, LNear) then
          begin
            Intersection := LNear;
            Result := True;
            Exit;
          end;
        end;
      end;
  end;
end;

procedure TMeshData.Render(const AContext: TContext3D; const AMaterial: TMaterial; const AOpacity: Single);
begin
  AContext.DrawTriangles(VertexBuffer, IndexBuffer, AMaterial, AOpacity);
end;

procedure TMeshData.ReadMesh(Stream: TStream);
var
  l: Cardinal;
begin
  BoundingBoxNeedsUpdate;
  Stream.Read(l, SizeOf(l));
  FVertexBuffer.Length := l;
  Stream.Read(FVertexBuffer.Buffer^, FVertexBuffer.Size);
  Stream.Read(l, SizeOf(l));
  FIndexBuffer.Length := l;
  Stream.Read(FIndexBuffer.Buffer^, FIndexBuffer.Size);
end;

procedure TMeshData.WriteMesh(Stream: TStream);
var
  l: Cardinal;
begin
  l := FVertexBuffer.Length;
  Stream.Write(l, SizeOf(l));
  Stream.Write(FVertexBuffer.Buffer^, FVertexBuffer.Size);
  l := FIndexBuffer.Length;
  Stream.Write(l, SizeOf(l));
  Stream.Write(FIndexBuffer.Buffer^, FIndexBuffer.Size);
end;

function TMeshData.GetBoundingBox: TBoundingBox;
var
  I: Integer;
  CurrentVertex: TPoint3D;
begin
  if FBoundingBoxUpdateNeeded then
  begin
    FBoundingBoxUpdateNeeded := False;
    if FVertexBuffer <> nil then
    begin
      if FVertexBuffer.Size = 0 then
        FBoundingBox := TBoundingBox.Empty
      else
      begin
        CurrentVertex := FVertexBuffer.GetVerticesPtr(0)^;
        FBoundingBox.MinCorner := CurrentVertex;
        FBoundingBox.MaxCorner := CurrentVertex;

        for I := 1 to (FVertexBuffer.Size div FVertexBuffer.FVertexSize) - 1 do
        begin
          CurrentVertex := FVertexBuffer.GetVerticesPtr(I)^;

          FBoundingBox.MinCorner.X := Min(CurrentVertex.X, FBoundingBox.MinCorner.X);
          FBoundingBox.MaxCorner.X := Max(CurrentVertex.X, FBoundingBox.MaxCorner.X);

          FBoundingBox.MinCorner.Y := Min(CurrentVertex.Y, FBoundingBox.MinCorner.Y);
          FBoundingBox.MaxCorner.Y := Max(CurrentVertex.Y, FBoundingBox.MaxCorner.Y);

          FBoundingBox.MinCorner.Z := Min(CurrentVertex.Z, FBoundingBox.MinCorner.Z);
          FBoundingBox.MaxCorner.Z := Max(CurrentVertex.Z, FBoundingBox.MaxCorner.Z);
        end;
      end;
    end
    else
      FBoundingBox := TBoundingBox.Empty;
  end;
  Result := FBoundingBox;
end;

function TMeshData.GetNormals: string;
var
  i: Integer;
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create(FVertexBuffer.Length * (3 * 12 + 4));
  try
    for i := 0 to (FVertexBuffer.Length - 1) do
    begin
      if i > 0 then
        SB.Append('  ');
      SB.Append(FloatToStr(FVertexBuffer.Normals[i].x, USFormatSettings));
      SB.Append(' ');
      SB.Append(FloatToStr(FVertexBuffer.Normals[i].y, USFormatSettings));
      SB.Append(' ');
      SB.Append(FloatToStr(FVertexBuffer.Normals[i].z, USFormatSettings));
    end;
    Result := SB.ToString(True);
  finally
    SB.Free;
  end;
end;

function TMeshData.GetPoint3Ds: string;
var
  i: Integer;
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create(FVertexBuffer.Length * (3 * 12 + 4));
  try
    for i := 0 to (FVertexBuffer.Length - 1) do
    begin
      if i > 0 then
        SB.Append('  ');
      SB.Append(FloatToStr(FVertexBuffer.Vertices[i].x, USFormatSettings));
      SB.Append(' ');
      SB.Append(FloatToStr(FVertexBuffer.Vertices[i].y, USFormatSettings));
      SB.Append(' ');
      SB.Append(FloatToStr(FVertexBuffer.Vertices[i].z, USFormatSettings));
    end;
    Result := SB.ToString(True);
  finally
    SB.Free;
  end;
end;

function TMeshData.GetTexCoordinates: string;
var
  i: Integer;
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create(FVertexBuffer.Length * (2 * 12 + 4));
  try
    for i := 0 to (FVertexBuffer.Length - 1) do
    begin
      if i > 0 then
        SB.Append('  ');
      SB.Append(FloatToStr(FVertexBuffer.TexCoord0[i].x, USFormatSettings));
      SB.Append(' ');
      SB.Append(FloatToStr(FVertexBuffer.TexCoord0[i].y, USFormatSettings));
    end;
    Result := SB.ToString(True);
  finally
    SB.Free;
  end;
end;

function TMeshData.GetTriangleIndices: string;
var
  i: Integer;
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create(FIndexBuffer.Length * 7);
  try
    for i := 0 to (FIndexBuffer.Length - 1) do
    begin
      SB.Append(FloatToStr(FIndexBuffer[i], USFormatSettings));
      SB.Append(' ');
      if (i + 1) mod 3 = 0 then
        SB.Append('  ');
    end;
    Result := SB.ToString(True).Trim;
  finally
    SB.Free;
  end;
end;

procedure TMeshData.BoundingBoxNeedsUpdate;
begin
  FBoundingBoxUpdateNeeded := True;
end;

procedure TMeshData.SetNormals(const Value: string);
var
  Pos, Count: Integer;
  Val: string;
begin
  // ensure a last separator
  Val := Value + ' ,';

  // calc size
  Pos := 0;
  Count := 0;
  while Pos < Val.Length do
    try
      Count := Count + 1;
      WideGetToken(Pos, Val, ' ,');
      WideGetToken(Pos, Val, ' ,');
      WideGetToken(Pos, Val, ' ,');
    except
    end;

  // fill
  FVertexBuffer.Length := Count;
  Pos := 0;
  Count := 0;
  while Pos < Val.Length do
    try
      Count := Count + 1;
      FVertexBuffer.Normals[Count - 1] := TPoint3D.Create(
        StrToFloat(WideGetToken(Pos, Val, ' ,'), USFormatSettings),
        StrToFloat(WideGetToken(Pos, Val, ' ,'), USFormatSettings),
        StrToFloat(WideGetToken(Pos, Val, ' ,'), USFormatSettings)
      );
    except
    end;

  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TMeshData.SetPoint3Ds(const Value: string);
var
  Pos, Count: Integer;
  Val: string;
begin
  BoundingBoxNeedsUpdate;

  // ensure a last separator
  Val := Value + ' ,';

  // calc size
  Count := 0;
  Pos := 0;
  while Pos < Val.Length do
  begin
    try
      Count := Count + 1;
      WideGetToken(Pos, Val, ' ,');
      WideGetToken(Pos, Val, ' ,');
      WideGetToken(Pos, Val, ' ,');
    except
    end;
  end;
  // fill
  FVertexBuffer.Length := Count;
  Count := 0;
  Pos := 0;
  while Pos < Val.Length do
    try
      Count := Count + 1;
      FVertexBuffer.Vertices[Count - 1] := TPoint3D.Create(
        StrToFloat(WideGetToken(Pos, Val, ' ,'), USFormatSettings),
        StrToFloat(WideGetToken(Pos, Val, ' ,'), USFormatSettings),
        StrToFloat(WideGetToken(Pos, Val, ' ,'), USFormatSettings)
      );
    except
    end;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TMeshData.SetTexCoordinates(const Value: string);
var
  Pos, Count: Integer;
  Val: string;
begin
  // ensure a last separator
  Val := Value + ' ';

  // calc size
  Count := 0;
  Pos := 0;
  while Pos < Val.Length do
    try
      Count := Count + 1;
      WideGetToken(Pos, Val, ' ,');
      WideGetToken(Pos, Val, ' ,');
    except
    end;
  // calc size
  FVertexBuffer.Length := Count;
  Count := 0;
  Pos := 0;
  while Pos < Val.Length do
    try
      Count := Count + 1;
      FVertexBuffer.TexCoord0[Count - 1] := PointF(
        StrToFloat(WideGetToken(Pos, Val, ' ,'), USFormatSettings),
        StrToFloat(WideGetToken(Pos, Val, ' ,'), USFormatSettings));
    except
    end;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TMeshData.SetTriangleIndices(const Value: string);
var
  Pos, Count: Integer;
  Val: string;
begin
  // ensure a last separator
  Val := Value + ' ,';

  // calc zise
  Count := 0;
  Pos := 0;
  while Pos < Val.Length do
    try
      Count := Count + 1;
      WideGetToken(Pos, Val, ' ,');
    except
    end;
  // fill
  FIndexBuffer.Length := Count;
  Count := 0;
  Pos := 0;
  while Pos < Val.Length do
    try
      Count := Count + 1;
      FIndexBuffer[Count - 1] := StrToInt(WideGetToken(Pos, Val, ' ,'));
    except
    end;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{ TMaterial }

constructor TMaterial.Create;
begin
  inherited;
  DoInitialize;
end;

destructor TMaterial.Destroy;
var
  I: Integer;
begin
  if FNotifyList <> nil then
  begin
    for I := FNotifyList.Count - 1 downto 0 do
      IFreeNotification(FNotifyList[I]).FreeNotification(Self);
    FreeAndNil(FNotifyList);
  end;
  inherited;
end;

procedure TMaterial.Apply(const Context: TContext3D);
begin
  DoApply(Context);
  FModified := False;
end;

procedure TMaterial.DoApply(const Context: TContext3D);
begin
end;

procedure TMaterial.DoChange;
begin
  FModified := True;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

class function TMaterial.DoGetMaterialProperty(const Prop: TMaterial.TProperty): string;
begin
  Result := '';
end;

class function TMaterial.GetMaterialProperty(const Prop: TProperty): string;
begin
  Result := DoGetMaterialProperty(Prop);
end;

procedure TMaterial.AddFreeNotify(const AObject: IFreeNotification);
begin
  if FNotifyList = nil then
    FNotifyList := TList<Pointer>.Create;
  FNotifyList.Add(Pointer(AObject));
end;

procedure TMaterial.RemoveFreeNotify(const AObject: IFreeNotification);
begin
  if FNotifyList <> nil then
    FNotifyList.Remove(Pointer(AObject));
end;

procedure TMaterial.Reset(const Context: TContext3D);
begin
  DoReset(Context);
end;

procedure TMaterial.DoReset(const Context: TContext3D);
begin
end;

{ TIndexBuffer }

constructor TIndexBuffer.Create(const ALength: Integer; const AFormat: TIndexFormat = TIndexFormat.UInt16);
begin
  inherited Create;
  FLength := ALength;
  FFormat := AFormat;
  if FFormat = TIndexFormat.UInt16 then
    FIndexSize := 2
  else
    FIndexSize := 4;
  FSize := FLength * FIndexSize;
  GetMem(FBuffer, Size);
end;

destructor TIndexBuffer.Destroy;
begin
  FreeMem(FBuffer);
  inherited;
end;

procedure TIndexBuffer.BeginDraw(const ALength: Integer);
begin
  FSaveLength := FLength;
  FLength := Min(ALength, FLength);
  FSize := FLength * FIndexSize;
end;

procedure TIndexBuffer.EndDraw;
begin
  FLength := FSaveLength;
  FSize := FLength * FIndexSize;
end;

procedure TIndexBuffer.Assign(Source: TPersistent);
begin
  if Source is TIndexBuffer then
  begin
    FreeMem(FBuffer);
    FFormat := TIndexBuffer(Source).FFormat;
    FLength := TIndexBuffer(Source).FLength;
    FSize := FLength * FIndexSize;
    GetMem(FBuffer, Size);
    Move(TIndexBuffer(Source).Buffer^, Buffer^, Size);
  end
  else
    inherited;
end;

function TIndexBuffer.GetIndices(AIndex: Integer): Integer;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if FFormat = TIndexFormat.UInt16 then
    Result := PWord(NativeInt(FBuffer) + AIndex * FIndexSize)^
  else
    Result := PCardinal(NativeInt(FBuffer) + AIndex * FIndexSize)^
end;

procedure TIndexBuffer.SetFormat(const Value: TIndexFormat);
begin
  if FFormat <> Value then
  begin
    FreeMem(FBuffer);
    FFormat := Value;
    if FFormat = TIndexFormat.UInt16 then
      FIndexSize := 2
    else
      FIndexSize := 4;
    FSize := FLength * FIndexSize;
    GetMem(FBuffer, Size);
  end;
end;

procedure TIndexBuffer.SetIndices(AIndex: Integer; const Value: Integer);
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if FFormat = TIndexFormat.UInt16 then
    PWord(NativeInt(FBuffer) + AIndex * FIndexSize)^:= Value
  else
    PCardinal(NativeInt(FBuffer) + AIndex * FIndexSize)^:= Value
end;

procedure TIndexBuffer.SetLength(const Value: Integer);
var
  Buf: Pointer;
  SaveLength: Integer;
begin
  if FLength <> Value then
  begin
    if FLength < Value then
      SaveLength := FLength
    else
      SaveLength := Value;
    GetMem(Buf, SaveLength * FIndexSize);
    try
      Move(FBuffer^, Buf^, SaveLength * FIndexSize);
      FreeMem(FBuffer);
      FLength := Value;
      FSize := FLength * FIndexSize;
      GetMem(FBuffer, FSize);
      Move(Buf^, FBuffer^, SaveLength * FIndexSize);
    finally
      FreeMem(Buf);
    end;
  end;
end;

{ TVertexBuffer }

constructor TVertexBuffer.Create(const AFormat: TVertexFormats;
  const ALength: Integer);
begin
  inherited Create;
  SetFormat(AFormat);
  FLength := ALength;
  FVertexSize := FMX.Types3D.VertexSize(FFormat);
  FSize := FVertexSize * FLength;
  GetMem(FBuffer, Size);
end;

destructor TVertexBuffer.Destroy;
begin
  FreeMem(FBuffer);
  inherited;
end;

procedure TVertexBuffer.BeginDraw(const ALength: Integer);
begin
  FSaveLength := FLength;
  FLength := Min(ALength, FLength);
  FSize := FVertexSize * FLength;
end;

procedure TVertexBuffer.EndDraw;
begin
  FLength := FSaveLength;
  FSize := FVertexSize * FLength;
end;

procedure TVertexBuffer.Assign(Source: TPersistent);
begin
  if Source is TVertexBuffer then
  begin
    FreeMem(FBuffer);
    SetFormat(TVertexBuffer(Source).FFormat);
    FLength := TVertexBuffer(Source).FLength;
    FVertexSize := FMX.Types3D.VertexSize(FFormat);
    FSize := FVertexSize * FLength;
    GetMem(FBuffer, Size);
    Move(TVertexBuffer(Source).Buffer^, Buffer^, Size);
  end
  else
    inherited;
end;

function TVertexBuffer.GetVertexDeclarations: TVertexDeclaration;
var
  F: TVertexFormat;
begin
  System.SetLength(Result, 0);
  for F := TVertexFormat.Vertex to TVertexFormat.Tangent do
  begin
    if F in Format then
    begin
      System.SetLength(Result, System.Length(Result) + 1);
      Result[High(Result)].Format := F;
      Result[High(Result)].Offset := GetVertexOffset(F, Format);
    end;
  end;
end;

function TVertexBuffer.GetVertices(AIndex: Integer): TPoint3D;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  Result := PPoint3D(@PByteArray(FBuffer)[AIndex * FVertexSize])^;
  {$R+}
end;

function TVertexBuffer.GetVerticesPtr(AIndex: Integer): PPoint3D;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  Result := PPoint3D(@PByteArray(FBuffer)[AIndex * FVertexSize]);
  {$R+}
end;

procedure TVertexBuffer.SetVertices(AIndex: Integer; const Value: TPoint3D);
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  PPoint3D(@PByteArray(FBuffer)[AIndex * FVertexSize])^ := Value;
  {$R+}
end;

function TVertexBuffer.GetTangents(AIndex: Integer): TPoint3D;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  Result := PPoint3D(@PByteArray(FBuffer)[AIndex * FVertexSize + FTangent])^;
  {$R+}
end;

function TVertexBuffer.GetTangentsPtr(AIndex: Integer): PPoint3D;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  Result := PPoint3D(@PByteArray(FBuffer)[AIndex * FVertexSize + FTangent]);
  {$R+}
end;

function TVertexBuffer.GetTexCoord0(AIndex: Integer): TPointF;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  Result := PPointF(@PByteArray(FBuffer)[AIndex * FVertexSize + FTexCoord0])^;
  {$R+}
end;

procedure TVertexBuffer.SetTangents(AIndex: Integer; const Value: TPoint3D);
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  PPoint3D(@PByteArray(FBuffer)[AIndex * FVertexSize + FTangent])^ := Value;
  {$R+}
end;

procedure TVertexBuffer.SetTexCoord0(AIndex: Integer; const Value: TPointF);
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  PPointF(@PByteArray(FBuffer)[AIndex * FVertexSize + FTexCoord0])^ := Value;
  {$R+}
end;

function TVertexBuffer.GetTexCoord1(AIndex: Integer): TPointF;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  Result := PPointF(@PByteArray(FBuffer)[AIndex * FVertexSize + FTexCoord1])^;
  {$R+}
end;

procedure TVertexBuffer.SetTexCoord1(AIndex: Integer; const Value: TPointF);
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  PPointF(@PByteArray(FBuffer)[AIndex * FVertexSize + FTexCoord1])^ := Value;
  {$R+}
end;

function TVertexBuffer.GetTexCoord2(AIndex: Integer): TPointF;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  Result := PPointF(@PByteArray(FBuffer)[AIndex * FVertexSize + FTexCoord2])^;
  {$R+}
end;

procedure TVertexBuffer.SetTexCoord2(AIndex: Integer; const Value: TPointF);
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  PPointF(@PByteArray(FBuffer)[AIndex * FVertexSize + FTexCoord2])^ := Value;
  {$R+}
end;

function TVertexBuffer.GetTexCoord3(AIndex: Integer): TPointF;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  Result := PPointF(@PByteArray(FBuffer)[AIndex * FVertexSize + FTexCoord3])^;
  {$R+}
end;

procedure TVertexBuffer.SetTexCoord3(AIndex: Integer; const Value: TPointF);
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  PPointF(@PByteArray(FBuffer)[AIndex * FVertexSize + FTexCoord3])^ := Value;
  {$R+}
end;

function TVertexBuffer.GetBiNormals(AIndex: Integer): TPoint3D;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  Result := PPoint3D(@PByteArray(FBuffer)[AIndex * FVertexSize + FBiNormal])^;
  {$R+}
end;

function TVertexBuffer.GetBiNormalsPtr(AIndex: Integer): PPoint3D;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  Result := PPoint3D(@PByteArray(FBuffer)[AIndex * FVertexSize + FBiNormal]);
  {$R+}
end;

procedure TVertexBuffer.SetBiNormals(AIndex: Integer; const Value: TPoint3D);
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  PPoint3D(@PByteArray(FBuffer)[AIndex * FVertexSize + FBiNormal])^ := Value;
  {$R+}
end;

function TVertexBuffer.GetColor0(AIndex: Integer): TAlphaColor;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  Result := PColor(@PByteArray(FBuffer)[AIndex * FVertexSize + FColor0])^;
  {$R+}
end;

function TVertexBuffer.GetItemPtr(AIndex: Integer): Pointer;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  Result := @PByteArray(FBuffer)[AIndex * FVertexSize];
  {$R+}
end;

procedure TVertexBuffer.SetLength(const Value: Integer);
var
  SaveLength: Integer;
  Buf: Pointer;
begin
  if FLength <> Value then
  begin
    if FLength < Value then
      SaveLength := FLength
    else
      SaveLength := Value;
    GetMem(Buf, SaveLength * FVertexSize);
    try
      Move(FBuffer^, Buf^, SaveLength * FVertexSize);
      FreeMem(FBuffer);
      FLength := Value;
      FSize := FLength * FVertexSize;
      GetMem(FBuffer, FSize);
      Move(Buf^, FBuffer^, SaveLength * FVertexSize);
    finally
      FreeMem(Buf);
    end;
  end;
end;

function TVertexBuffer.GetNormals(AIndex: Integer): TPoint3D;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  Result := PPoint3D(@PByteArray(FBuffer)[AIndex * FVertexSize + FNormal])^;
  {$R+}
end;

function TVertexBuffer.GetNormalsPtr(AIndex: Integer): PPoint3D;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  Result := PPoint3D(@PByteArray(FBuffer)[AIndex * FVertexSize + FNormal]);
  {$R+}
end;

procedure TVertexBuffer.SetNormals(AIndex: Integer; const Value: TPoint3D);
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  PPoint3D(@PByteArray(FBuffer)[AIndex * FVertexSize + FNormal])^ := Value;
  {$R+}
end;

function TVertexBuffer.GetColor1(AIndex: Integer): TAlphaColor;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  if TVertexFormat.ColorF1 in Format then
    Result := PAlphaColorF(@PByteArray(FBuffer)[AIndex * FVertexSize + FColorF1]).ToAlphaColor
  else
    Result := PColor(@PByteArray(FBuffer)[AIndex * FVertexSize + FColor1])^
  {$R+}
end;

function TVertexBuffer.GetColor2(AIndex: Integer): TAlphaColor;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  if TVertexFormat.ColorF2 in Format then
    Result := PAlphaColorF(@PByteArray(FBuffer)[AIndex * FVertexSize + FColorF2]).ToAlphaColor
  else
    Result := PColor(@PByteArray(FBuffer)[AIndex * FVertexSize + FColor2])^;
  {$R+}
end;

function TVertexBuffer.GetColor3(AIndex: Integer): TAlphaColor;
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  if TVertexFormat.ColorF3 in Format then
    Result := PAlphaColorF(@PByteArray(FBuffer)[AIndex * FVertexSize + FColorF3]).ToAlphaColor
  else
    Result := PColor(@PByteArray(FBuffer)[AIndex * FVertexSize + FColor3])^;
  {$R+}
end;

procedure TVertexBuffer.SetColor0(AIndex: Integer; const Value: TAlphaColor);
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  if TVertexFormat.ColorF0 in Format then
    PAlphaColorF(@PByteArray(FBuffer)[AIndex * FVertexSize + FColorF0])^ := TAlphaColorF.Create(Value)
  else
    PAlphaColor(@PByteArray(FBuffer)[AIndex * FVertexSize + FColor0])^ := RGBtoBGR(Value);
  {$R+}
end;

procedure TVertexBuffer.SetColor1(AIndex: Integer; const Value: TAlphaColor);
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  if TVertexFormat.ColorF1 in Format then
    PAlphaColorF(@PByteArray(FBuffer)[AIndex * FVertexSize + FColorF1])^ := TAlphaColorF.Create(Value)
  else
    PAlphaColor(@PByteArray(FBuffer)[AIndex * FVertexSize + FColor1])^ := Value;
  {$R+}
end;

procedure TVertexBuffer.SetColor2(AIndex: Integer; const Value: TAlphaColor);
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  if TVertexFormat.ColorF2 in Format then
    PAlphaColorF(@PByteArray(FBuffer)[AIndex * FVertexSize + FColorF2])^ := TAlphaColorF.Create(Value)
  else
    PColor(@PByteArray(FBuffer)[AIndex * FVertexSize + FColor2])^ := Value;
  {$R+}
end;

procedure TVertexBuffer.SetColor3(AIndex: Integer; const Value: TAlphaColor);
begin
  if (AIndex < 0) or (AIndex >= Length) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  {$R-}
  if TVertexFormat.ColorF3 in Format then
    PAlphaColorF(@PByteArray(FBuffer)[AIndex * FVertexSize + FColorF3])^ := TAlphaColorF.Create(Value)
  else
    PColor(@PByteArray(FBuffer)[AIndex * FVertexSize + FColor3])^ := Value;
  {$R+}
end;

procedure TVertexBuffer.SetFormat(Value: TVertexFormats);
begin
  if FFormat <> Value then
  begin
    FFormat := Value;
    FTexCoord0 := GetVertexOffset(TVertexFormat.TexCoord0, FFormat);
    FTexCoord1 := GetVertexOffset(TVertexFormat.TexCoord1, FFormat);
    FTexCoord2 := GetVertexOffset(TVertexFormat.TexCoord2, FFormat);
    FTexCoord3 := GetVertexOffset(TVertexFormat.TexCoord3, FFormat);
    FColor0 := GetVertexOffset(TVertexFormat.Color0, FFormat);
    FColor1 := GetVertexOffset(TVertexFormat.Color1, FFormat);
    FColor2 := GetVertexOffset(TVertexFormat.Color2, FFormat);
    FColor3 := GetVertexOffset(TVertexFormat.Color3, FFormat);
    FNormal := GetVertexOffset(TVertexFormat.Normal, FFormat);
    FBiNormal := GetVertexOffset(TVertexFormat.BiNormal, FFormat);
    FTangent := GetVertexOffset(TVertexFormat.Tangent, FFormat);
    FColorF0 := GetVertexOffset(TVertexFormat.ColorF0, FFormat);
    FColorF1 := GetVertexOffset(TVertexFormat.ColorF1, FFormat);
    FColorF2 := GetVertexOffset(TVertexFormat.ColorF2, FFormat);
    FColorF3 := GetVertexOffset(TVertexFormat.ColorF3, FFormat);
  end;
end;

{ TContextShaderVariable }

constructor TContextShaderVariable.Create(const Name: string; const Kind: TContextShaderVariableKind; const Index, Size: Integer);
begin
  Self.Name := Name;
  Self.Kind := Kind;
  Self.Index := Index;
  Self.Size := Size;
end;

{ TContextShaderSource }

constructor TContextShaderSource.Create(const Arch: TContextShaderArch; const ACode: array of Byte;
   const AVariables: array of TContextShaderVariable);
var
  I: Integer;
begin
  Self.Arch := Arch;
  SetLength(Self.Code, Length(ACode));
  if Length(Code) > 0 then
    Move(ACode[0], Self.Code[0], Length(Code));
  SetLength(Self.Variables, Length(AVariables));
  for I := 0 to High(AVariables) do
    Self.Variables[I] := AVariables[I];
end;

function TContextShaderSource.IsDefined: Boolean;
begin
  Result := Arch <> TContextShaderArch.Undefined;
end;

function TContextShaderSource.FindVariable(const AName: string; out AShaderVariable: TContextShaderVariable): Boolean;
var
  I: Integer;
begin
  Result := False;
  if IsDefined then
    for I := 0 to High(Variables) do
      if SameText(Variables[I].Name, AName) then
      begin
        AShaderVariable := Variables[I];
        Exit(True);
      end;
end;

{ TContextShader }

const
  ShaderSignature: array[0..3] of Byte = ($46, $4D, $53, $43); // FMSC

constructor TContextShader.Create;
begin
  inherited Create;
  if TContextStyle.Fragile in TContextManager.DefaultContextClass.Style then
    FContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, ContextLostHandler)
  else
    FContextLostId := -1;
end;

destructor TContextShader.Destroy;
begin
  if FContextLostId <> -1 then
    TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FContextLostId);
  TShaderManager.UnregisterShader(Self);
  inherited;
end;

procedure TContextShader.ContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin
  TContextManager.FinalizeShader(Self);
end;

class function TContextShader.BuildKey(const Name: string; const Kind: TContextShaderKind;
  const Sources: array of TContextShaderSource): string;
var
  I: Integer;
  S: TStringBuilder;
begin
  Result := '';
  S := TStringBuilder.Create;
  try
    S.Append(Name);
    S.Append(IntToStr(Integer(Kind)));
    for I := 0 to High(Sources) do
      S.Append('-').Append(IntToStr(Length(Sources[I].Code))).Append('-').Append(IntToStr(Length(Sources[I].Variables)));
    Result := S.ToString(True);
  finally
    S.Free;
  end;
end;

function TContextShader.GetSourceByArch(Arch: TContextShaderArch): TContextShaderSource;
var
  I: Integer;
begin
  Result := TContextShaderSource.Create(TContextShaderArch.Undefined, [], []);
  for I := 0 to High(FSources) do
    if FSources[I].Arch = Arch then
    begin
      Result := FSources[I];
      Break;
    end;
end;

procedure TContextShader.LoadFromData(const Name: string; const Kind: TContextShaderKind; const OriginalSource: string;
  const Sources: array of TContextShaderSource);
var
  I: Integer;
begin
  FKind := Kind;
  FName := Name;
  FOriginalSource := OriginalSource;
  SetLength(FSources, Length(Sources));
  for I := 0 to High(FSources) do
    FSources[I] := Sources[I];
end;

procedure TContextShader.LoadFromFile(const FileName: string);
var
  S: TStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TContextShader.LoadFromStream(const AStream: TStream);
var
  R: TReader;
  Len, I, J: Integer;
  Sign: array of Byte;
begin
  R := TReader.Create(AStream, 1024);
  try
    SetLength(Sign, 4);
    R.Read(Sign[0], SizeOf(ShaderSignature));
    if (Sign[0] = ShaderSignature[0]) and (Sign[1] = ShaderSignature[1]) and (Sign[2] = ShaderSignature[2]) and (Sign[3] = ShaderSignature[3]) then
    begin
      R.Read(FKind, SizeOf(FKind));
      FName := R.ReadString;
      FOriginalSource := R.ReadString;
      Len := R.ReadInteger;
      SetLength(FSources, Len);
      for I := 0 to High(FSources) do
      begin
        R.Read(FSources[I].Arch, SizeOf(FSources[I].Arch));
        Len := R.ReadInteger;
        SetLength(FSources[I].Code, Len);
        if Len > 0 then
          R.Read(FSources[I].Code[0], Len);
        Len := R.ReadInteger;
        SetLength(FSources[I].Variables, Len);
        for J := 0 to High(FSources[I].Variables) do
        begin
          FSources[I].Variables[J].Name := R.ReadString;
          R.Read(FSources[I].Variables[J].Kind, SizeOf(FSources[I].Variables[J].Kind));
          FSources[I].Variables[J].Index := R.ReadInteger;
          FSources[I].Variables[J].Size := R.ReadInteger;
        end;
      end;
    end;
  finally
    R.Free;
  end;
end;

procedure TContextShader.SaveToFile(const FileName: string);
var
  S: TStream;
begin
  S := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TContextShader.SaveToStream(const AStream: TStream);
var
  W: TWriter;
  I, J: Integer;
  Source: TContextShaderSource;
begin
  W := TWriter.Create(AStream, 1024);
  try
    W.Write(ShaderSignature[0], SizeOf(ShaderSignature));
    W.Write(FKind, SizeOf(FKind));
    W.WriteString(FName);
    W.WriteString(FOriginalSource);
    W.WriteInteger(Length(FSources));
    for I := 0 to High(FSources) do
    begin
      Source := FSources[I];
      W.Write(Source.Arch, SizeOf(Source.Arch));
      W.WriteInteger(Length(Source.Code));
      W.Write(Source.Code[0], Length(Source.Code));
      W.WriteInteger(Length(Source.Variables));
      for J := 0 to High(Source.Variables) do
      begin
        W.WriteString(Source.Variables[J].Name);
        W.Write(Source.Variables[J].Kind, SizeOf(Source.Variables[J].Kind));
        W.WriteInteger(Source.Variables[J].Index);
        W.WriteInteger(Source.Variables[J].Size);
      end;
    end;
  finally
    W.Free;
  end;
end;

{ TShaderManager }

class function TShaderManager.GetShader(const Key: string): TContextShader;
begin
  Result := nil;
  if FShaderList <> nil then
    FShaderList.TryGetValue(Key, Result)
end;

class function TShaderManager.RegisterShader(const Shader: TContextShader): TContextShader;
var
  S: TContextShader;
  Key: string;
begin
  if Shader <> nil then
  begin
    Key := TContextShader.BuildKey(Shader.Name, Shader.Kind, Shader.FSources);
    if FShaderList <> nil then
      FShaderList.TryGetValue(Key, S)
    else
      S := nil;
    if S <> nil then
    begin
      S.FRefCount := S.FRefCount + 1;
      Exit(S);
    end
    else
    begin
      if FShaderList = nil then
        FShaderList := TObjectDictionary<string, TContextShader>.Create([doOwnsValues]);
      FShaderList.AddOrSetValue(Key, Shader);
      Result := Shader;
      Shader.FRefCount := Shader.FRefCount + 1;
    end;
  end
  else
    Result := nil;
end;

class function TShaderManager.RegisterShaderFromData(const Name: string; const Kind: TContextShaderKind; const OriginalSource: string;
  const Sources: array of TContextShaderSource): TContextShader;
var
  S: TContextShader;
begin
  S := GetShader(TContextShader.BuildKey(Name, Kind, Sources));
  if S <> nil then
    Result := S
  else
  begin
    S := TContextShader.Create;
    S.LoadFromData(Name, Kind, OriginalSource, Sources);
    RegisterShader(S);
    Result := S;
  end;
end;

class function TShaderManager.RegisterShaderFromFile(const FileName: string): TContextShader;
var
  S: TContextShader;
begin
  S := TContextShader.Create;
  S.LoadFromFile(FileName);
  Result := GetShader(TContextShader.BuildKey(S.Name, S.FKind, S.FSources));
  if Result <> nil then
  begin
    S.Free;
  end else begin
    RegisterShader(S);
    Result := S;
  end;
end;

class procedure TShaderManager.UnregisterShader(const Shader: TContextShader);
var
  Key: string;
begin
  if Shader <> nil then
  begin
    Shader.FRefCount := Shader.FRefCount - 1;
    if Shader.FRefCount = 0 then
    begin
      if FShaderList <> nil then
      begin
        Key := TContextShader.BuildKey(Shader.Name, Shader.Kind, Shader.FSources);
        FShaderList.Remove(Key);
      end;
      if Shader.Handle <> 0 then
        TContextManager.FinalizeShader(Shader);
    end;
  end;
end;

class procedure TShaderManager.UnInitialize;
begin
  FreeAndNil(FShaderList);
end;

{ TTexture }

constructor TTexture.Create;
begin
  inherited Create;
  FTextureScale := 1.0;
  FMinFilter := TTextureFilter.Linear;
  FMagFilter := TTextureFilter.Linear;
  FPixelFormat := TPixelFormat.None;
  FStyle := [TTextureStyle.MipMaps, TTextureStyle.Dynamic];
  if TContextStyle.Fragile in TContextManager.DefaultContextClass.Style then
  begin
    FContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, ContextLostHandler);
    FContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, ContextResetHandler);
  end
  else
  begin
    FContextLostId := -1;
    FContextResetId := -1;
    Include(FStyle, TTextureStyle.Volatile);
  end;
end;

destructor TTexture.Destroy;
begin
  if FContextLostId <> -1 then
    TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FContextLostId);
  if FContextResetId <> -1 then
    TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, FContextResetId);
  TContextManager.DefaultContextClass.FinalizeTexture(Self);
  if FBits <> nil then
    FreeMem(FBits);
  inherited;
end;

procedure TTexture.ContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin
  if not (TTextureStyle.Volatile in Style) then
  begin
    if FHandle <> 0 then
      FRequireInitializeAfterLost := True;
    Finalize;
  end;
end;

procedure TTexture.ContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin
  if not (TTextureStyle.Volatile in Style) then
  begin
    if FRequireInitializeAfterLost then
      Initialize;
    FRequireInitializeAfterLost := False;
    if FBits <> nil then
      UpdateTexture(FBits, Width * BytesPerPixel);
  end;
end;

procedure TTexture.Initialize;
begin
  TContextManager.DefaultContextClass.InitializeTexture(Self);
end;

procedure TTexture.Finalize;
begin
  TContextManager.DefaultContextClass.FinalizeTexture(Self);
end;

function TTexture.IsEmpty: Boolean;
begin
  Result := FWidth * FHeight = 0;
end;

procedure TTexture.LoadFromStream(const Stream: TStream);
var
  Surf: TBitmapSurface;
begin
  if not (TTextureStyle.RenderTarget in Style) then
  begin
    Surf := TBitmapSurface.Create;
    try
      if TBitmapCodecManager.LoadFromStream(Stream, Surf) then
        Assign(Surf)
    finally
      Surf.Free;
    end;
  end;
end;

procedure TTexture.Assign(Source: TPersistent);
var
  M: TBitmapData;
begin
  TMonitor.Enter(Self);
  try
    if Source is TBitmap then
    begin
      if FHandle <> 0 then
        TContextManager.DefaultContextClass.FinalizeTexture(Self);
      FPixelFormat := TBitmap(Source).PixelFormat;
      FStyle := [TTextureStyle.Dynamic];
      FTextureScale := TBitmap(Source).BitmapScale;
      SetSize(TBitmap(Source).Width, TBitmap(Source).Height);
      if not (TCanvasStyle.NeedGPUSurface in TBitmap(Source).CanvasClass.GetCanvasStyle) then
      begin
        if TBitmap(Source).Map(TMapAccess.Read, M) then
        try
          UpdateTexture(M.Data, M.Pitch);
        finally
          TBitmap(Source).Unmap(M);
        end;
      end;
    end else if Source is TBitmapSurface then
    begin
      if FHandle <> 0 then
        TContextManager.DefaultContextClass.FinalizeTexture(Self);
      FStyle := [TTextureStyle.Dynamic];
      SetSize(TBitmapSurface(Source).Width, TBitmapSurface(Source).Height);
      UpdateTexture(TBitmapSurface(Source).Bits, TBitmapSurface(Source).Pitch);
    end else
      inherited ;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TTexture.UpdateTexture(const Bits: Pointer; const Pitch: Integer);
begin
  TMonitor.Enter(Self);
  try
    if not (TTextureStyle.Volatile in Style) then
      if TContextStyle.Fragile in TContextManager.DefaultContextClass.Style then
      begin
        if FBits = nil then
          GetMem(FBits, Pitch * Height);
        Move(Bits^, FBits^, Pitch * Height);
      end;
    TContextManager.DefaultContextClass.UpdateTexture(Self, Bits, Pitch);
  finally
    TMonitor.Exit(Self);
  end;
end;

function TTexture.GetBytesPerPixel: Integer;
begin
  if FPixelFormat = TPixelFormat.None then
    raise ECannotFindSuitablePixelFormat.CreateResFmt(@SCannotFindSuitablePixelFormat, [ClassName]);
  Result := PixelFormatBytes[FPixelFormat];
end;

procedure TTexture.SetHandle(const AHandle: THandle);
begin
  FHandle := AHandle;
end;

procedure TTexture.SetHeight(const Value: Integer);
begin
  SetSize(Width, Value);
end;

procedure TTexture.SetMagFilter(const Value: TTextureFilter);
begin
  FMagFilter := Value;
end;

procedure TTexture.SetMinFilter(const Value: TTextureFilter);
begin
  FMinFilter := Value;
end;

procedure TTexture.SetPixelFormat(const Value: TPixelFormat);
begin
  FPixelFormat := Value;
end;

procedure TTexture.SetSize(const AWidth, AHeight: Integer);
begin
  TMonitor.Enter(Self);
  try
    Finalize;
    FWidth := AWidth;
    FHeight := AHeight;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TTexture.SetStyle(const Value: TTextureStyles);
begin
  FStyle := Value;
end;

procedure TTexture.SetTextureScale(const Scale: Single);
begin
  FTextureScale := Scale;
end;

procedure TTexture.SetWidth(const Value: Integer);
begin
  SetSize(Value, Height);
end;

{ TTextureBitmap }

procedure TTextureBitmap.BitmapChanged;
var
  M: TBitmapData;
begin
  inherited;
  if not (TCanvasStyle.NeedGPUSurface in CanvasClass.GetCanvasStyle) and (FTexture <> nil) and
    Map(TMapAccess.Read, M) then
  try
    FTexture.UpdateTexture(M.Data, M.Pitch);
  finally
    Unmap(M);
  end;
end;

procedure TTextureBitmap.DestroyResources;
begin
  inherited;
  if (FTexture <> nil) and not (TCanvasStyle.NeedGPUSurface in CanvasClass.GetCanvasStyle) then
    FTexture.Free;
  FTexture := nil;
end;

function TTextureBitmap.GetTexture: TTexture;
begin
  if (TCanvasStyle.NeedGPUSurface in CanvasClass.GetCanvasStyle) or (FTexture = nil) then
    FTexture := TContextManager.DefaultContextClass.BitmapToTexture(Self);
  Result := FTexture;
end;

{ TLightDescription }

constructor TLightDescription.Create(AEnabled: Boolean; AColor: TAlphaColor; ALightType: TLightType; ASpotCutOff: Single;
      ASpotExponent: Single; APosition: TPoint3D; ADirection: TPoint3D);
begin
  Enabled := AEnabled;
  Color := AColor;
  LightType := ALightType;
  SpotCutOff := ASpotCutOff;
  SpotExponent := ASpotExponent;
  Position := APosition;
  Direction := ADirection;
end;

{ TContext3D }

constructor TContext3D.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
  const AMultisample: TMultisample; const ADepthStencil: Boolean);
begin
  inherited Create;
  FParent := AParent;
  FMultisample := AMultisample;
  FDepthStencil := ADepthStencil;
  FWidth := AWidth;
  FHeight := AHeight;
  InitContext;
end;

constructor TContext3D.CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample; const ADepthStencil: Boolean);
begin
  inherited Create;
  FTexture := ATexture;
  FMultisample := AMultisample;
  FDepthStencil := ADepthStencil;
  FWidth := FTexture.Width;
  FHeight := FTexture.Height;
  InitContext;
  if FTexture.Handle = 0 then
    FTexture.Initialize;
end;

procedure TContext3D.InitContext;
begin
  FScale := GetContextScale;
  if FTimerService = nil then
    if not TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
      raise EUnsupportedPlatformService.Create('IFMXTimerService');
  if FContextCount = 0 then
  begin
    FSaveStates := TList<TContextStates>.Create;
    FCurrentMatrix := TMatrix3D.Identity;
    FCurrentScissorRect := Rect(0, 0, 0, 0);
    ResetStates;
  end;
  Inc(FContextCount);
  FRecalcScreenMatrix := True;
  FRecalcProjectionMatrix := True;
  FCenterOffset := TPosition.Create(PointF(0.0, 0.0));
  FRenderToMatrix := TMatrix3D.Identity;
  FDefaultMaterial := TColorMaterial.Create;
  FCurrentCameraMatrix := TMatrix3D.Identity;
  FCurrentCameraInvMatrix := FCurrentCameraMatrix.Inverse;
  FCurrentAngleOfView := RadToDeg(cPI / 4);
  FLights := TLightDescriptionList.Create;
  TColorMaterial(FDefaultMaterial).Color := DefaultMaterialColor;
end;

destructor TContext3D.Destroy;
begin
  FreeBuffer;
  FreeAndNil(FLights);
  FreeAndNil(FDefaultMaterial);
  FreeAndNil(FCenterOffset);
  Dec(FContextCount);
  if FContextCount = 0 then
    FreeAndNil(FSaveStates);
  inherited;
end;

function TContext3D.GetContextScale: Single;
begin
  if Parent <> nil then
    Result := Parent.Scale
  else if Texture <> nil then
    Result := Texture.TextureScale
  else
    Result := DefaultScale;
end;

class procedure TContext3D.ResetStates;
begin
  FillChar(FCurrentStates, SizeOf(FCurrentStates), 0);
  FCurrentOpacity := 0;
  FCurrentFormat := [];
  FCurrentMaterial := nil;
  FCurrentMaterialClass := nil;
end;

procedure TContext3D.Clear(const AColor: TAlphaColor);
begin
  Clear([TClearTarget.Color], AColor, 0, 0);
end;

procedure TContext3D.Clear(const ATarget: TClearTargets; const AColor: TAlphaColor; const ADepth: single; const AStencil: Cardinal);
begin
  DoClear(ATarget, AColor, ADepth, AStencil);
end;

function TContext3D.GetCurrentState(AIndex: TContextState): Boolean;
begin
  Result := FCurrentStates[AIndex];
end;

function TContext3D.GetProjectionMatrix: TMatrix3D;
var
  M: TMatrix3D;
begin
  if FRecalcProjectionMatrix then
  begin
    if SameValue(FHeight, 0.0, Epsilon) then
      Result := TMatrix3D.CreatePerspectiveFovRH(DegToRad(FCurrentAngleOfView), 1.0, 1.0, 1000.0)
    else
      Result := TMatrix3D.CreatePerspectiveFovRH(DegToRad(FCurrentAngleOfView), FWidth / FHeight, 1.0, 1000.0);
    if (FRenderToMatrix.m41 <> 0) or (FRenderToMatrix.m11 <> 1) then
      Result := Result * FRenderToMatrix;
    M := TMatrix3D.Identity;
    M.m41 := FCenterOffset.X;
    M.m42 := FCenterOffset.Y;
    FProjectionMatrix := Result * M;
    FInvProjectionMatrix := FProjectionMatrix.Inverse;
    FRecalcProjectionMatrix := False;
  end;
  Result := FProjectionMatrix;
end;

function TContext3D.GetCurrentModelViewProjectionMatrix: TMatrix3D;
var
  ScaleMatrix: TMatrix3D;
begin
  if CurrentStates[TContextState.cs3DScene] then
    Result := CurrentCameraMatrix * CurrentProjectionMatrix
  else
    Result := CurrentScreenMatrix;

  if CurrentStates[TContextState.cs2DScene] then
    if (Texture <> nil) and (Texture.TextureScale > 1.0) then
    begin
      ScaleMatrix := TMatrix3D.Identity;
      ScaleMatrix.m11 := Texture.TextureScale;
      ScaleMatrix.m22 := Texture.TextureScale;
      Result := ScaleMatrix * Result;
    end;

  Result := CurrentMatrix * Result;
end;

function TContext3D.GetScreenMatrix: TMatrix3D;
var
  matProj, scaleMatrix, transMatrix, orthoProj: TMatrix3D;
begin
  if FRecalcScreenMatrix then
  begin
    if (FRenderToMatrix.m41 <> 0) or (FRenderToMatrix.m11 <> 1) then
    begin
      Result := FRenderToMatrix;
    end
    else
    begin
      orthoProj := TMatrix3D.CreateOrthoOffCenterRH(0, FHeight, FWidth, 0, 1, 1000);

      if SameValue(FHeight, 0.0, Epsilon) then
        matProj := TMatrix3D.CreatePerspectiveFovRH(cPI / 6, 1, 1, 1000)
      else
        matProj := TMatrix3D.CreatePerspectiveFovRH(cPI / 6, FWidth / FHeight, 1, 1000);

      transMatrix := TMatrix3D.Identity;
      transMatrix.m41 := 0;
      transMatrix.m42 := 0;
      transMatrix.m43 := -2;
      matProj := transMatrix * matProj;

      scaleMatrix := TMatrix3D.Identity;
      scaleMatrix.m11 := (orthoProj.m11 / matProj.m11) * 2;
      scaleMatrix.m22 := -(orthoProj.m11 / matProj.m11) * 2;
      scaleMatrix.m33 := -(orthoProj.m11 / matProj.m11) * 2;
      matProj := scaleMatrix * matProj;

      transMatrix := TMatrix3D.Identity;
      transMatrix.m41 := -FWidth / 2;
      transMatrix.m42 := -FHeight / 2;
      transMatrix.m43 := 0;
      matProj := transMatrix * matProj;

      Result := matProj;
    end;

    FScreenMatrix := Result;
    FInvScreenMatrix := FScreenMatrix.Inverse;
    FRecalcScreenMatrix := False;
  end
  else
    Result := FScreenMatrix;
end;

procedure TContext3D.CreateBuffer;
begin
  FRecalcScreenMatrix := True;
  FRecalcProjectionMatrix := True;
  DoCreateBuffer;
end;

procedure TContext3D.Resize;
begin
  DoResize;
end;

procedure TContext3D.FreeBuffer;
begin
  DoFreeBuffer;
end;

procedure TContext3D.FreeNotification(AObject: TObject);
begin
  if AObject = FCurrentMaterial then
    FCurrentMaterial := nil;
end;

class function TContext3D.DoBitmapToTexture(const Bitmap: TBitmap): TTexture;
begin
  if Bitmap <> nil then
  begin
    Result := TTexture.Create;
    Result.Assign(Bitmap);
  end else
    Result := nil;
end;

function TContext3D.DoBeginScene: Boolean;
begin
  Result := True;
end;

procedure TContext3D.DoEndScene;
begin
  if GlobalBeginSceneCount > 1 then
    PopContextStates;
end;

procedure TContext3D.AssignTo(Dest: TPersistent);
begin
  if Dest is TBitmap then
  begin
    TBitmap(Dest).SetSize(Width, Height);
    CopyToBitmap(TBitmap(Dest), Rect(0, 0, Width, Height));
  end
  else
    inherited;
end;

function TContext3D.BeginScene: Boolean;
begin
  TCanvas.Lock;
  try
    if FGlobalBeginSceneCount = 0 then
    begin
      FChangeStateCount := 0;
      FChangeShaderCount := 0;
      if (FTimerService <> nil) and (Texture = nil) then
        FBeginTime := FTimerService.GetTick;
    end;
    Inc(FGlobalBeginSceneCount);
    if FBeginSceneCount = 0 then
    begin
      if GlobalBeginSceneCount > 1 then
        PushContextStates;
      FCurrentContext := Self;
      Result := DoBeginScene;
      if Result then
      begin
        FRecalcScreenMatrix := True;
        FRecalcProjectionMatrix := True;
        SetMatrix(TMatrix3D.Identity);
      end
      else
        TCanvas.Unlock;
    end
    else
      Result := FBeginSceneCount > 0;
    if Result then
      Inc(FBeginSceneCount);
  except
    TCanvas.Unlock;
    raise;
  end;
end;

class function TContext3D.BitmapToTexture(const Bitmap: TBitmap): TTexture;
begin
  Result := DoBitmapToTexture(Bitmap);
end;

procedure TContext3D.EndScene;
begin
  try
    if FBeginSceneCount = 1 then
      DoEndScene;
    if FBeginSceneCount > 0 then
      Dec(FBeginSceneCount);
    Dec(FGlobalBeginSceneCount);
    if FGlobalBeginSceneCount = 0 then
    begin
      FCurrentContext := nil;
      if (FTimerService <> nil) and (Texture = nil) then
      begin
        FEndTime := FTimerService.GetTick;
        FRenderTime := FRenderTime + (FEndTime - FBeginTime);
        FFrameCount := FFrameCount + 1;
        if (FFrameCount > 10) and (FRenderTime > 0) then
        begin
          FFPS := FFrameCount / FRenderTime;
          FRenderTime := 0;
          FFrameCount := 0;
        end;
      end;
    end;
  finally
    TCanvas.Unlock;
  end;
end;

procedure TContext3D.SetStateFromContext(const AContext: TContext3D);
var
  I: Integer;
begin
  FCurrentCameraMatrix := AContext.FCurrentCameraMatrix;
  FCurrentCameraInvMatrix := AContext.FCurrentCameraInvMatrix;
  FLights.Clear;
  for I := 0 to AContext.FLights.Count - 1 do
    FLights.Add(AContext.FLights[I]);
end;

procedure TContext3D.PopContextStates;
var
  State: TContextStates;
  S: TContextState;
begin
  if FSaveStates.Count > 0 then
  begin
    State := FSaveStates[FSaveStates.Count - 1];
    SetMatrix(State.Matrix);
    for S := Low(TContextState) to High(TContextState) do
      if State.States[S] then
        SetContextState(S);
    FCurrentContext := State.Context;
    if FCurrentStates[TContextState.csScissorOn] and (FCurrentContext <> nil) then
      FCurrentContext.SetScissorRect(State.ScissorRect);

    FSaveStates.Delete(FSaveStates.Count - 1);
  end;
end;

procedure TContext3D.PushContextStates;
var
  State: TContextStates;
begin
  State.States := FCurrentStates;
  State.Matrix := FCurrentMatrix;
  State.Context := FCurrentContext;
  State.ScissorRect := FCurrentScissorRect;

  FSaveStates.Add(State);
end;

procedure TContext3D.SetContextState(const State: TContextState);
begin
  if not FCurrentStates[State] then
  begin
    FCurrentStates[State] := True;
    case State of
      TContextState.cs2DScene:
        FCurrentStates[TContextState.cs3DScene] := False;
      TContextState.cs3DScene:
        FCurrentStates[TContextState.cs2DScene] := False;
      TContextState.csZTestOn:
        FCurrentStates[TContextState.csZTestOff] := False;
      TContextState.csZTestOff:
        FCurrentStates[TContextState.csZTestOn] := False;
      TContextState.csZWriteOn:
        FCurrentStates[TContextState.csZWriteOff] := False;
      TContextState.csZWriteOff:
        FCurrentStates[TContextState.csZWriteOn] := False;
      TContextState.csAlphaBlendOn:
        FCurrentStates[TContextState.csAlphaBlendOff] := False;
      TContextState.csAlphaBlendOff:
        FCurrentStates[TContextState.csAlphaBlendOn] := False;
      TContextState.csStencilOn:
        FCurrentStates[TContextState.csStencilOff] := False;
      TContextState.csStencilOff:
        FCurrentStates[TContextState.csStencilOn] := False;
      TContextState.csColorWriteOn:
        FCurrentStates[TContextState.csColorWriteOff] := False;
      TContextState.csColorWriteOff:
        FCurrentStates[TContextState.csColorWriteOn] := False;
      TContextState.csScissorOn:
        FCurrentStates[TContextState.csScissorOff] := False;
      TContextState.csScissorOff:
        FCurrentStates[TContextState.csScissorOn] := False;
      TContextState.csFrontFace:
        begin
          FCurrentStates[TContextState.csBackFace] := False;
          FCurrentStates[TContextState.csAllFace] := False;
        end;
      TContextState.csBackFace:
        begin
          FCurrentStates[TContextState.csAllFace] := False;
          FCurrentStates[TContextState.csFrontFace] := False;
        end;
      TContextState.csAllFace:
        begin
          FCurrentStates[TContextState.csBackFace] := False;
          FCurrentStates[TContextState.csFrontFace] := False;
        end;
    end;
    DoSetContextState(State);
    FChangeStateCount := FChangeStateCount + 1;
  end;
end;

procedure TContext3D.SetStencilFunc(const Func: TStencilfunc; Ref, Mask: Cardinal);
begin
  DoSetStencilFunc(Func, Ref, Mask);
end;

procedure TContext3D.SetStencilOp(const Fail, ZFail, ZPass: TStencilOp);
begin
  DoSetStencilOp(Fail, ZFail, ZPass);
end;

class function TContext3D.Style: TContextStyles;
begin
  Result := [];
end;

class function TContext3D.TextureUnitCount: Integer;
begin
  Result := DefaultTextureUnitCount;
end;

procedure TContext3D.SetShaderVariable(const Name: string; const Matrix: TMatrix3D);
begin
  DoSetShaderVariable(Name, Matrix);
end;

procedure TContext3D.SetShaders(const VertexShader, PixelShader: TContextShader);
begin
  if (VertexShader <> nil) and (PixelShader <> nil) then
  begin
    if VertexShader.Handle = 0 then
      TContextManager.InitializeShader(VertexShader);
    if PixelShader.Handle = 0 then
      TContextManager.InitializeShader(PixelShader);
    DoSetShaders(VertexShader, PixelShader);
    FCurrentVertexShader := VertexShader;
    FCurrentPixelShader := PixelShader;
    FChangeShaderCount := FChangeShaderCount + 1;
  end;
end;

procedure TContext3D.SetShaderVariable(const Name: string; const Color: TAlphaColor);
begin
  SetShaderVariable(Name, [Vector3D(
    TAlphaColorRec(Color).R / $FF,
    TAlphaColorRec(Color).G / $FF,
    TAlphaColorRec(Color).B / $FF,
    TAlphaColorRec(Color).A / $FF
  )]);
end;

procedure TContext3D.SetShaderVariable(const Name: string; const Texture: TTexture);
begin
  DoSetShaderVariable(Name, Texture);
end;

procedure TContext3D.SetShaderVariable(const Name: string; const Data: array of TVector3D);
begin
  DoSetShaderVariable(Name, Data);
end;

procedure TContext3D.SetSize(const AWidth, AHeight: Integer);
begin
  if (FWidth <> AWidth) or (FHeight <> AHeight) then
  begin
    FreeBuffer;
    FWidth := AWidth;
    FHeight := AHeight;
    if FWidth < 1 then FWidth := 1;
    if FHeight < 1 then FHeight := 1;
    Resize;
    // clear matrix state
    FCurrentStates[TContextState.cs2DScene] := False;
    FCurrentStates[TContextState.cs3DScene] := False;
    //
    CreateBuffer;
  end;
end;

procedure TContext3D.SetMultisample(const Multisample: TMultisample);
begin
  if FMultisample <> Multisample then
  begin
    FreeBuffer;
    FMultisample := Multisample;
    CreateBuffer;
  end;
end;

procedure TContext3D.SetRenderToMatrix(const Matrix: TMatrix3D);
begin
  FRenderToMatrix := Matrix;
end;

procedure TContext3D.SetMatrix(const M: TMatrix3D);
begin
  FCurrentMatrix := M;
end;

procedure TContext3D.SetCameraAngleOfView(const Angle: Single);
begin
  FCurrentAngleOfView := Angle;
end;

procedure TContext3D.SetCameraMatrix(const M: TMatrix3D);
begin
  FCurrentCameraMatrix := M;
  FCurrentCameraInvMatrix := FCurrentCameraMatrix.Inverse;
end;

procedure TContext3D.SetScissorRect(const ScissorRect: TRect);
begin
  FCurrentScissorRect := ScissorRect;
  DoSetScissorRect(FCurrentScissorRect);
end;

procedure TContext3D.Pick(X, Y: Single; const AProj: TProjection; var RayPos, RayDir: TVector3D);
var
  MatProj: TMatrix3D;
  VecPos, VecNear: TPoint3D;
begin
  if AProj = TProjection.Camera then
  begin
    MatProj := GetProjectionMatrix;

    // Compute the vector of the pick ray in screen space
    VecPos := NullPoint3D;
    if FTexture <> nil then
      VecNear := TPoint3D.Create((1 + (FCenterOffset.X) - (2 * ((X * FTexture.TextureScale) / FWidth))) / MatProj.m11,
        - (1 - FCenterOffset.Y - (2 * ((Y * FTexture.TextureScale) / FHeight))) / MatProj.m22, 1)
    else
      VecNear := TPoint3D.Create((1 + (FCenterOffset.X) - (2 * (X / FWidth))) / MatProj.m11, - (1 - FCenterOffset.Y -
        (2 * (Y / FHeight))) / MatProj.m22, 1);

    // Transform the screen space pick ray into 3D space
    VecPos := VecPos * CurrentCameraInvMatrix;
    VecNear := VecNear * CurrentCameraInvMatrix;

    RayPos := VecPos;
    RayDir := (VecPos - VecNear).Normalize;
    RayDir.W := 0;
  end
  else
  begin
    GetScreenMatrix; // force recalculation if need

    VecPos := NullPoint3D * FInvScreenMatrix;

    VecPos := TPoint3D.Create(FWidth / 2, FHeight / 2, VecPos.Z * 2);
    VecNear := TPoint3D.Create(X, Y, 0);

    RayPos := VecPos;
    RayDir := (VecNear - VecPos).Normalize;
    RayDir.W := 0.0;
  end;
end;

class function TContext3D.PixelToPixelPolygonOffset: TPointF;
begin
  Result := TPointF.Zero;
end;

function TContext3D.WorldToScreen(const AProj: TProjection; const P: TPoint3D): TPoint3D;
var
  MatProj: TMatrix3D;
begin
  if AProj = TProjection.Camera then
  begin
    Result := TPoint3D(TVector3D.Create(P) * FCurrentCameraMatrix);
    MatProj := GetProjectionMatrix;

    if not SameValue(Result.Z, TEpsilon.Matrix) then
    begin
      Result.X := -((Result.X / Result.Z) * MatProj.m11 - 1) * FWidth / 2;
      Result.Y := ((Result.Y / Result.Z) * MatProj.m22 + 1) * FHeight / 2;
    end;
  end
  else
    Result := P;
end;

procedure TContext3D.ApplyMaterial(const Material: TMaterial);
var
  M: TMatrix3D;
begin
  if Material <> nil then
  begin
    FCurrentMaterialClass := TMaterialClass(Material.ClassType);
    Material.Apply(Self);
    if Material.GetMaterialProperty(TMaterial.TProperty.ModelViewProjection) <> '' then
      SetShaderVariable(Material.GetMaterialProperty(TMaterial.TProperty.ModelViewProjection), CurrentModelViewProjectionMatrix);
    if Material.GetMaterialProperty(TMaterial.TProperty.ModelView) <> '' then
      SetShaderVariable(Material.GetMaterialProperty(TMaterial.TProperty.ModelView), CurrentMatrix);
    if Material.GetMaterialProperty(TMaterial.TProperty.ModelViewInverseTranspose) <> '' then
    begin
      M := CurrentMatrix.Inverse.Transpose;
      SetShaderVariable(Material.GetMaterialProperty(TMaterial.TProperty.ModelViewInverseTranspose), M);
    end;
  end;
end;

procedure TContext3D.ResetMaterial(const Material: TMaterial);
begin
  if Material <> nil then
    Material.Reset(Self);
end;

procedure TContext3D.DrawPoints(const Vertices: TVertexBuffer; const Indices: TIndexBuffer; const Material: TMaterial; const Opacity: Single);
begin
  FCurrentOpacity := Opacity;
  if Material <> nil then
    FCurrentMaterial := Material
  else
  begin
    FCurrentMaterial := DefaultMaterial;
    if FCurrentMaterial is TColorMaterial then
      TColorMaterial(FCurrentMaterial).Color := MakeColor(DefaultMaterialColor, Opacity);
  end;
  ApplyMaterial(FCurrentMaterial);
  DoDrawPrimitives(TPrimitivesKind.Points, Vertices.Buffer, Indices.Buffer, Vertices.GetVertexDeclarations,
    Vertices.VertexSize, Vertices.Length, Indices.IndexSize, Indices.Length);
  ResetMaterial(FCurrentMaterial);
end;

procedure TContext3D.DrawCube(const Center, Size: TPoint3D; const Opacity: Single; const Color: TAlphaColor);
var
  I: Integer;
  A, B: TPoint3D;
  Ver: TVertexBuffer;
  Idx: TIndexBuffer;
  Mat: TColorMaterial;
begin
  Idx := nil;
  Ver := nil;
  Mat := nil;
  try
    Ver := TVertexBuffer.Create([TVertexFormat.Vertex, TVertexFormat.TexCoord0], 24);
    Idx := TIndexBuffer.Create(24);
    A := Center - (Size * 0.5);
    B := Center + (Size * 0.5);
    begin
      Ver.Vertices[0] := TPoint3D.Create(A.X, A.Y, B.Z);
      Ver.TexCoord0[0] := TPointF.Zero;
      Ver.Vertices[1] := TPoint3D.Create(B.X, A.Y, B.Z);
      Ver.TexCoord0[1] := TPointF.Create(1, 0);
      Ver.Vertices[2] := TPoint3D.Create(A.X, A.Y, A.Z);
      Ver.TexCoord0[2] := TPointF.Zero;
      Ver.Vertices[3] := TPoint3D.Create(B.X, A.Y, A.Z);
      Ver.TexCoord0[3] := TPointF.Create(1, 0);
      Ver.Vertices[4] := TPoint3D.Create(A.X, B.Y, B.Z);
      Ver.TexCoord0[4] := TPointF.Zero;
      Ver.Vertices[5] := TPoint3D.Create(B.X, B.Y, B.Z);
      Ver.TexCoord0[5] := TPointF.Create(1, 0);
      Ver.Vertices[6] := TPoint3D.Create(A.X, B.Y, A.Z);
      Ver.TexCoord0[6] := TPointF.Zero;
      Ver.Vertices[7] := TPoint3D.Create(B.X, B.Y, A.Z);
      Ver.TexCoord0[7] := TPointF.Create(1, 0);

      Ver.Vertices[8] := TPoint3D.Create(A.X, A.Y, A.Z);
      Ver.Vertices[9] := TPoint3D.Create(A.X, B.Y, A.Z);
      Ver.Vertices[10] := TPoint3D.Create(A.X, A.Y, B.Z);
      Ver.Vertices[11] := TPoint3D.Create(A.X, B.Y, B.Z);
      Ver.Vertices[12] := TPoint3D.Create(B.X, A.Y, A.Z);
      Ver.Vertices[13] := TPoint3D.Create(B.X, B.Y, A.Z);
      Ver.Vertices[14] := TPoint3D.Create(B.X, A.Y, B.Z);
      Ver.Vertices[15] := TPoint3D.Create(B.X, B.Y, B.Z);

      Ver.Vertices[16] := TPoint3D.Create(A.X, A.Y, A.Z);
      Ver.Vertices[17] := TPoint3D.Create(A.X, A.Y, B.Z);
      Ver.Vertices[18] := TPoint3D.Create(B.X, A.Y, A.Z);
      Ver.Vertices[19] := TPoint3D.Create(B.X, A.Y, B.Z);
      Ver.Vertices[20] := TPoint3D.Create(A.X, B.Y, A.Z);
      Ver.Vertices[21] := TPoint3D.Create(A.X, B.Y, B.Z);
      Ver.Vertices[22] := TPoint3D.Create(B.X, B.Y, A.Z);
      Ver.Vertices[23] := TPoint3D.Create(B.X, B.Y, B.Z);
    end;
    for I := 0 to Ver.Length - 1 do
      Idx[I] := I;
    Mat := TColorMaterial.Create;
    Mat.Color := Color;
    DrawLines(Ver, Idx, Mat, Opacity);
  finally
    Idx.Free;
    Ver.Free;
    Mat.Free;
  end;
end;

procedure TContext3D.DrawLine(const StartPoint, EndPoint: TPoint3D; const Opacity: Single; const Color: TAlphaColor);
var
  Ver: TVertexBuffer;
  Idx: TIndexBuffer;
  Mat: TColorMaterial;
begin
  Idx := nil;
  Ver := nil;
  Mat := nil;
  try
    Ver := TVertexBuffer.Create([TVertexFormat.Vertex], 2);
    Ver.Vertices[0] := StartPoint;
    Ver.Vertices[1] := EndPoint;
    Idx := TIndexBuffer.Create(2);
    Idx[0] := 0;
    Idx[1] := 1;
    Mat := TColorMaterial.Create;
    Mat.Color := Color;
    DrawLines(Ver, Idx, Mat, Opacity);
  finally
    Idx.Free;
    Ver.Free;
    Mat.Free;
  end;
end;
procedure TContext3D.DrawLines(const Vertices: TVertexBuffer; const Indices: TIndexBuffer; const Material: TMaterial; const Opacity: Single);
begin
  FCurrentOpacity := Opacity;
  if Material <> nil then
    FCurrentMaterial := Material
  else
  begin
    FCurrentMaterial := DefaultMaterial;
    if FCurrentMaterial is TColorMaterial then
      TColorMaterial(FCurrentMaterial).Color := MakeColor(DefaultMaterialColor, Opacity);
  end;
  ApplyMaterial(FCurrentMaterial);
  DoDrawPrimitives(TPrimitivesKind.Lines, Vertices.Buffer, Indices.Buffer, Vertices.GetVertexDeclarations,
    Vertices.VertexSize, Vertices.Length, Indices.IndexSize, Indices.Length);
  ResetMaterial(FCurrentMaterial);
end;

procedure TContext3D.DrawTriangles(const Vertices: TVertexBuffer; const Indices: TIndexBuffer; const Material: TMaterial; const Opacity: Single);
begin
  FCurrentOpacity := Opacity;
  if Material <> nil then
    FCurrentMaterial := Material
  else begin
    FCurrentMaterial := DefaultMaterial;
    if FCurrentMaterial is TColorMaterial then
      TColorMaterial(FCurrentMaterial).Color := MakeColor(DefaultMaterialColor, Opacity);
  end;
  ApplyMaterial(FCurrentMaterial);
  DoDrawPrimitives(TPrimitivesKind.Triangles, Vertices.Buffer, Indices.Buffer, Vertices.GetVertexDeclarations,
    Vertices.VertexSize, Vertices.Length, Indices.IndexSize, Indices.Length);
  ResetMaterial(FCurrentMaterial);
end;

procedure TContext3D.DrawPrimitives(const AKind: TPrimitivesKind; const Vertices, Indices: Pointer;
  const VertexDeclaration: TVertexDeclaration; const VertexSize, VertexCount, IndexSize, IndexCount: Integer;
  const Material: TMaterial; const Opacity: Single);
begin
  FCurrentOpacity := Opacity;
  if Material <> nil then
    FCurrentMaterial := Material
  else
  begin
    FCurrentMaterial := DefaultMaterial;
    if FCurrentMaterial is TColorMaterial then
      TColorMaterial(FCurrentMaterial).Color := MakeColor(DefaultMaterialColor, Opacity);
  end;
  ApplyMaterial(FCurrentMaterial);
  DoDrawPrimitives(AKind, Vertices, Indices, VertexDeclaration, VertexSize, VertexCount, IndexSize, IndexCount);
  ResetMaterial(FCurrentMaterial);
end;

class procedure TContext3D.InitializeShader(const Shader: TContextShader);
begin
  if Shader <> nil then
    DoInitializeShader(Shader);
end;

procedure TContext3D.FillCube(const Center, Size: TPoint3D; const Opacity: Single; const Color: TAlphaColor);
var
  Ver: TVertexBuffer;
  Idx: TIndexBuffer;
  Mat: TColorMaterial;
  TX1, TY1, TX2, TY2: Single;
  A, B, N: TPoint3D;
  I: Integer;
begin
  Idx := nil;
  Ver := nil;
  Mat := nil;
  try
    A := Center - (Size * 0.5);
    B := Center + (Size * 0.5);
    TX1 := 0;
    TY1 := 0;
    TX2 := 1;
    TY2 := 1;
    // Front
    N := -((TPoint3D.Create(A.X, A.Y, B.Z) - TPoint3D.Create(B.X, A.Y, B.Z)).CrossProduct(TPoint3D.Create(A.X, A.Y,
      B.Z) - TPoint3D.Create(B.X, A.Y, A.Z)));
    Ver := TVertexBuffer.Create([TVertexFormat.Vertex, TVertexFormat.Normal, TVertexFormat.TexCoord0], 24);
    Ver.Vertices[0] := TPoint3D.Create(A.X, A.Y, B.Z);
    Ver.Normals[0] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[0] := TPointF.Create(TX1, TY1);
    Ver.Vertices[1] := TPoint3D.Create(B.X, A.Y, B.Z);
    Ver.Normals[1] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[1] := TPointF.Create(TX2, TY1);
    Ver.Vertices[2] := TPoint3D.Create(B.X, A.Y, A.Z);
    Ver.Normals[2] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[2] := TPointF.Create(TX2, TY2);
    Ver.Vertices[3] := TPoint3D.Create(A.X, A.Y, A.Z);
    Ver.Normals[3] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[3] := TPointF.Create(TX1, TY2);
    // Right
    N := -(TPoint3D.Create(B.X, A.Y, B.Z) - TPoint3D.Create(B.X, B.Y, B.Z)).CrossProduct(
      TPoint3D.Create(B.X, A.Y, B.Z) - TPoint3D.Create(B.X, B.Y, A.Z));
    Ver.Vertices[4] := TPoint3D.Create(B.X, A.Y, B.Z);
    Ver.Normals[4] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[4] := TPointF.Create(TX1, TY1);
    Ver.Vertices[5] := TPoint3D.Create(B.X, B.Y, B.Z);
    Ver.Normals[5] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[5] := TPointF.Create(TX2, TY1);
    Ver.Vertices[6] := TPoint3D.Create(B.X, B.Y, A.Z);
    Ver.Normals[6] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[6] := TPointF.Create(TX2, TY2);
    Ver.Vertices[7] := TPoint3D.Create(B.X, A.Y, A.Z);
    Ver.Normals[7] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[7] := TPointF.Create(TX1, TY2);
    // Left
    N := -(TPoint3D.Create(A.X, B.Y, B.Z) - TPoint3D.Create(A.X, A.Y, B.Z)).CrossProduct(
      TPoint3D.Create(A.X, B.Y, B.Z) - TPoint3D.Create(A.X, A.Y, A.Z));
    Ver.Vertices[8] := TPoint3D.Create(A.X, B.Y, B.Z);
    Ver.Normals[8] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[8] := TPointF.Create(TX1, TY1);
    Ver.Vertices[9] := TPoint3D.Create(A.X, A.Y, B.Z);
    Ver.Normals[9] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[9] := TPointF.Create(TX2, TY1);
    Ver.Vertices[10] := TPoint3D.Create(A.X, A.Y, A.Z);
    Ver.Normals[10] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[10] := TPointF.Create(TX2, TY2);
    Ver.Vertices[11] := TPoint3D.Create(A.X, B.Y, A.Z);
    Ver.Normals[11] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[11] := TPointF.Create(TX1, TY2);
    // Back
    N := -(TPoint3D.Create(B.X, B.Y, B.Z) - TPoint3D.Create(A.X, B.Y, B.Z)).CrossProduct(
      TPoint3D.Create(B.X, B.Y, B.Z) - TPoint3D.Create(A.X, B.Y, A.Z));
    Ver.Vertices[12] := TPoint3D.Create(B.X, B.Y, B.Z);
    Ver.Normals[12] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[12] := TPointF.Create(TX1, TY1);
    Ver.Vertices[13] := TPoint3D.Create(A.X, B.Y, B.Z);
    Ver.Normals[13] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[13] := TPointF.Create(TX2, TY1);
    Ver.Vertices[14] := TPoint3D.Create(A.X, B.Y, A.Z);
    Ver.Normals[14] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[14] := TPointF.Create(TX2, TY2);
    Ver.Vertices[15] := TPoint3D.Create(B.X, B.Y, A.Z);
    Ver.Normals[15] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[15] := TPointF.Create(TX1, TY2);
    // Top
    N := -(TPoint3D.Create(A.X, B.Y, B.Z) - TPoint3D.Create(B.X, B.Y, B.Z)).CrossProduct(
      TPoint3D.Create(A.X, B.Y, B.Z) - TPoint3D.Create(B.X, A.Y, B.Z));
    Ver.Vertices[16] := TPoint3D.Create(A.X, B.Y, B.Z);
    Ver.Normals[16] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[16] := TPointF.Create(TX1, TY1);
    Ver.Vertices[17] := TPoint3D.Create(B.X, B.Y, B.Z);
    Ver.Normals[17] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[17] := TPointF.Create(TX2, TY1);
    Ver.Vertices[18] := TPoint3D.Create(B.X, A.Y, B.Z);
    Ver.Normals[18] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[18] := TPointF.Create(TX2, TY2);
    Ver.Vertices[19] := TPoint3D.Create(A.X, A.Y, B.Z);
    Ver.Normals[19] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[19] := TPointF.Create(TX1, TY2);
    // Bottom
    N := -(TPoint3D.Create(A.X, A.Y, A.Z) - TPoint3D.Create(B.X, A.Y, A.Z)).CrossProduct(
      TPoint3D.Create(A.X, A.Y, A.Z) - TPoint3D.Create(B.X, B.Y, A.Z));
    Ver.Vertices[20] := TPoint3D.Create(A.X, A.Y, A.Z);
    Ver.Normals[20] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[20] := TPointF.Create(TX1, TY1);
    Ver.Vertices[21] := TPoint3D.Create(B.X, A.Y, A.Z);
    Ver.Normals[21] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[21] := TPointF.Create(TX2, TY1);
    Ver.Vertices[22] := TPoint3D.Create(B.X, B.Y, A.Z);
    Ver.Normals[22] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[22] := TPointF.Create(TX2, TY2);
    Ver.Vertices[23] := TPoint3D.Create(A.X, B.Y, A.Z);
    Ver.Normals[23] := TPoint3D.Create(N.X, N.Y, N.Z);
    Ver.TexCoord0[23] := TPointF.Create(TX1, TY2);

    // Indices
    Idx := TIndexBuffer.Create(36);
    for I := 0 to 5 do
    begin
      Idx[I * 6 + 0] := (I * 4) + 0;
      Idx[I * 6 + 1] := (I * 4) + 1;
      Idx[I * 6 + 2] := (I * 4) + 3;
      Idx[I * 6 + 3] := (I * 4) + 3;
      Idx[I * 6 + 4] := (I * 4) + 1;
      Idx[I * 6 + 5] := (I * 4) + 2;
    end;

    Mat := TColorMaterial.Create;
    Mat.Color := Color;
    DrawTriangles(Ver, Idx, Mat, Opacity);
  finally
    Idx.Free;
    Ver.Free;
    Mat.Free;
  end;
end;

procedure TContext3D.FillPolygon(const Center, Size: TPoint3D; const Rect: TRectF; const Points: TPolygon;
  const Material: TMaterial; const Opacity: Single; Front, Back, Left: Boolean);
var
  VertexBuffer: TVertexBuffer;
  IndexBuffer: TIndexBuffer;
  MaxLimit, MinLimit: TPoint3D;
  CurrentPoint: TPointF;
  RelativePoint: TPoint3D;
  I, J: Integer;
  StartIndex: Integer;
  LeftLen, CurPos: Single;
  Index, VertexIndex1, VertexIndex2, VertexIndex3: Integer;
  PrevVertexIndex1, PrevVertexIndex2, PrevVertexIndex3: Integer;
  FaceVector1, FaceVector2: TPoint3D;
  PrevFaceVector1, PrevFaceVector2: TPoint3D;
  Normal, PreviousNormal, CurrentNormal: TPoint3D;
  Vertex1, Vertex2, Vertex3: TPoint3D;
begin
  if (Length(Points) = 0) or SameValue(Size.X, 0, TEpsilon.Scale) or SameValue(Size.Y, 0, TEpsilon.Scale) then
    Exit;

  MaxLimit := TPoint3D.Create($FFFF, $FFFF, 0);
  MinLimit := TPoint3D.Create(-$FFFF, -$FFFF, 0);
  LeftLen := 0;

  for I := 0 to High(Points) do
  begin
    if (Points[I].X >= $FFFF) and (Points[I].Y >= $FFFF) then
      Continue;

    CurrentPoint := Points[I];
    MaxLimit.X := Min(MaxLimit.X, CurrentPoint.X);
    MaxLimit.Y := Min(MaxLimit.Y, CurrentPoint.Y);

    MinLimit.X := Max(MinLimit.X, CurrentPoint.X);
    MinLimit.Y := Max(MinLimit.Y, CurrentPoint.Y);

    if Left and (I > 0) then
      if Points[I - 1].X >= $FFFF then
      begin
        if (I > 1) then
          LeftLen := LeftLen + TPointF.Create(X - Points[I - 2].X, Y - Points[I - 2].Y).Length;
      end
      else
        LeftLen := LeftLen + TPointF.Create(X - Points[I - 1].X, Y - Points[I - 1].Y).Length;
  end;

  if not IsRectEmpty(Rect) then
  begin
    MaxLimit.X := Min(MaxLimit.X, Rect.Left);
    MaxLimit.Y := Min(MaxLimit.Y, Rect.Top);

    MinLimit.X := Max(MinLimit.X, Rect.Right);
    MinLimit.Y := Max(MinLimit.Y, Rect.Bottom);
  end;

  if SameValue(MaxLimit.X, MinLimit.X, TEpsilon.Position) then
    Exit;
  if SameValue(MaxLimit.Y, MinLimit.Y, TEpsilon.Position) then
    Exit;

  VertexBuffer := TVertexBuffer.Create([TVertexFormat.Vertex, TVertexFormat.Normal, TVertexFormat.TexCoord0], 0);
  IndexBuffer := TIndexBuffer.Create(0, TIndexFormat.UInt32);

  // Front face
  if Front then
  begin
    VertexBuffer.Length := Length(Points);
    // set vertices
    for I := 0 to High(Points) do
    begin
      if (Points[I].X >= $FFFF) and (Points[I].Y >= $FFFF) then
      begin
        VertexBuffer.Vertices[I] := TPoint3D.Zero;
        VertexBuffer.Normals[I] := TPoint3D.Zero;
        VertexBuffer.TexCoord0[I] := TPointF.Zero;
        Continue;
      end;
      RelativePoint := TPoint3D.Create((Points[I].X - MaxLimit.X) / Abs(MinLimit.X - MaxLimit.X),
        (Points[I].Y - MaxLimit.Y) / Abs(MinLimit.Y - MaxLimit.Y), 1);
      VertexBuffer.Vertices[I] := TPoint3D.Create(Center.X - (Size.X / 2) + (RelativePoint.X * Size.X),
        Center.Y - (Size.Y / 2) + (RelativePoint.Y * Size.Y), Center.Z - (Size.Z / 2) + (RelativePoint.Z * Size.Z));
      VertexBuffer.Normals[I] := TPoint3D.Create(0, 0, 1);
      VertexBuffer.TexCoord0[I] := TPointF.Create(0.02 + (X * 0.96), 0.02 + (Y * 0.96));
    end;
    // Set indices
    IndexBuffer.Length := High(Points) * 3;
    StartIndex := 0;
    J := 0;
    for I := 0 to High(Points) - 1 do
    begin
      if (Points[I].X >= $FFFF) and (Points[I].Y >= $FFFF) then
      begin
        StartIndex := I + 1;
        Continue;
      end;
      IndexBuffer[(J * 3) + 0] := StartIndex;
      IndexBuffer[(J * 3) + 1] := I + 1;
      IndexBuffer[(J * 3) + 2] := I;
      Inc(J);
    end;
    IndexBuffer.Length := (J - 1) * 3;
    // Write to stencil
    SetContextState(TContextState.csStencilOn);
    Clear([TClearTarget.Stencil], 0, 0, 0);
    SetContextState(TContextState.csColorWriteOff);
    SetContextState(TContextState.csZWriteOff);
    SetStencilFunc(TStencilFunc.Always, 0, $FF);
    SetStencilOp(TStencilOp.Keep, TStencilOp.Keep, TStencilOp.Invert);
    SetContextState(TContextState.csAllFace);
    DrawTriangles(VertexBuffer, IndexBuffer, Material, 1);
    SetContextState(TContextState.csZWriteOn);
    SetContextState(TContextState.csColorWriteOn);
    // Just paint rect using stencil
    VertexBuffer.Length := 4;
    VertexBuffer.Vertices[0] := TPoint3D.Create(Center.X - (Size.X / 2), Center.Y - (Size.Y / 2), Center.Z + (Size.Z / 2));
    VertexBuffer.Normals[0] := TPoint3D.Create(0, 0, 1);
    VertexBuffer.TexCoord0[0] := TPointF.Zero;

    VertexBuffer.Vertices[1] := TPoint3D.Create(Center.X + (Size.X / 2), Center.Y - (Size.Y / 2), Center.Z + (Size.Z / 2));
    VertexBuffer.Normals[1] := TPoint3D.Create(0, 0, 1);
    VertexBuffer.TexCoord0[1] := TPointF.Create(1, 0);

    VertexBuffer.Vertices[2] := TPoint3D.Create(Center.X + (Size.X / 2), Center.Y + (Size.Y / 2), Center.Z + (Size.Z / 2));
    VertexBuffer.Normals[2] := TPoint3D.Create(0, 0, 1);
    VertexBuffer.TexCoord0[2] := TPointF.Create(1, 1);

    VertexBuffer.Vertices[3] := TPoint3D.Create(Center.X - (Size.X / 2), Center.Y + (Size.Y / 2), Center.Z + (Size.Z / 2));
    VertexBuffer.Normals[3] := TPoint3D.Create(0, 0, 1);
    VertexBuffer.TexCoord0[3] := TPointF.Create(0, 1);
    // Indices
    IndexBuffer.Length := 6;
    IndexBuffer[0] := 0;
    IndexBuffer[1] := 3;
    IndexBuffer[2] := 1;
    IndexBuffer[3] := 1;
    IndexBuffer[4] := 3;
    IndexBuffer[5] := 2;
    SetStencilFunc(TStencilFunc.NotEqual, 0, $FF);
    SetStencilOp(TStencilOp.Keep, TStencilOp.Keep, TStencilOp.Keep);
    SetContextState(TContextState.csFrontFace);
    DrawTriangles(VertexBuffer, IndexBuffer, Material, Opacity);
    SetContextState(TContextState.csStencilOff);
  end;

  // Back Face
  if Back then
  begin
    VertexBuffer.Length := Length(Points);
    // set vertices
    for I := 0 to High(Points) do
    begin
      if (Points[I].X >= $FFFF) and (Points[I].Y >= $FFFF) then
      begin
        VertexBuffer.Vertices[I] := TPoint3D.Zero;
        VertexBuffer.Normals[I] := TPoint3D.Zero;
        VertexBuffer.TexCoord0[I] := TPointF.Zero;
        Continue;
      end;

      RelativePoint := TPoint3D.Create((Points[I].X - MaxLimit.X) / Abs(MinLimit.X - MaxLimit.X),
        (Points[I].Y - MaxLimit.Y) / Abs(MinLimit.Y - MaxLimit.Y), 0);

      VertexBuffer.Vertices[I] := TPoint3D.Create(Center.X - (Size.X / 2) + (RelativePoint.X * Size.X),
        Center.Y - (Size.Y / 2) + (RelativePoint.Y * Size.Y), Center.Z - (Size.Z / 2) + (RelativePoint.Z * Size.Z));
      VertexBuffer.Normals[I] := TPoint3D.Create(0, 0, -1);
      VertexBuffer.TexCoord0[I] := TPointF.Create(0.02 + (X * 0.96), 0.02 + (Y * 0.96));
    end;

    // Set indices
    IndexBuffer.Length := High(Points) * 3;
    StartIndex := 0;
    J := 0;
    for I := 0 to High(Points) - 1 do
    begin
      if (Points[I].X >= $FFFF) and (Points[I].Y >= $FFFF) then
      begin
        StartIndex := I + 1;
        Continue;
      end;

      IndexBuffer[(J * 3) + 0] := StartIndex;
      IndexBuffer[(J * 3) + 1] := I + 1;
      IndexBuffer[(J * 3) + 2] := I;
      Inc(J);
    end;
    IndexBuffer.Length := (J - 1) * 3;
    // write to stencil
    SetContextState(TContextState.csStencilOn);
    Clear([TClearTarget.Stencil], 0, 0, 0);
    SetContextState(TContextState.csColorWriteOff);
    SetContextState(TContextState.csZWriteOff);
    SetStencilFunc(TStencilFunc.Always, 0, $FF);
    SetStencilOp(TStencilOp.Keep, TStencilOp.Keep, TStencilOp.Invert);
    SetContextState(TContextState.csAllFace);
    DrawTriangles(VertexBuffer, IndexBuffer, Material, 1);
    SetContextState(TContextState.csZWriteOn);
    SetContextState(TContextState.csColorWriteOn);
    // just paint rect using stencil
    VertexBuffer.Length := 4;
    VertexBuffer.Vertices[0] := TPoint3D.Create(Center.X - (Size.X / 2), Center.Y - (Size.Y / 2), Center.Z - (Size.Z / 2));
    VertexBuffer.Normals[0] := TPoint3D.Create(0, 0, -1);
    VertexBuffer.TexCoord0[0] := TPointF.Zero;

    VertexBuffer.Vertices[1] := TPoint3D.Create(Center.X + (Size.X / 2), Center.Y - (Size.Y / 2), Center.Z - (Size.Z / 2));
    VertexBuffer.Normals[1] := TPoint3D.Create(0, 0, -1);
    VertexBuffer.TexCoord0[1] := TPointF.Create(1, 0);

    VertexBuffer.Vertices[2] := TPoint3D.Create(Center.X + (Size.X / 2), Center.Y + (Size.Y / 2), Center.Z - (Size.Z / 2));
    VertexBuffer.Normals[2] := TPoint3D.Create(0, 0, -1);
    VertexBuffer.TexCoord0[2] := TPointF.Create(1, 1);

    VertexBuffer.Vertices[3] := TPoint3D.Create(Center.X - (Size.X / 2), Center.Y + (Size.Y / 2), Center.Z - (Size.Z / 2));
    VertexBuffer.Normals[3] := TPoint3D.Create(0, 0, -1);
    VertexBuffer.TexCoord0[3] := TPointF.Create(0, 1);
    // Indices
    IndexBuffer.Length := 6;
    IndexBuffer[0] := 0;
    IndexBuffer[1] := 1;
    IndexBuffer[2] := 3;
    IndexBuffer[3] := 1;
    IndexBuffer[4] := 2;
    IndexBuffer[5] := 3;
    SetStencilFunc(TStencilFunc.NotEqual, 0, $FF);
    SetStencilOp(TStencilOp.Keep, TStencilOp.Keep, TStencilOp.Keep);
    SetContextState(TContextState.csFrontFace);
    DrawTriangles(VertexBuffer, IndexBuffer, Material, Opacity);
    SetContextState(TContextState.csStencilOff);
  end;

  // sides
  if Left and (LeftLen > 0) then
  begin
    VertexBuffer.Length := Length(Points) * 2;
    // set vertices
    CurPos := 0;
    for I := 0 to High(Points) do
    begin
      if (Points[I].X >= $FFFF) and (Points[I].Y >= $FFFF) then
      begin
        VertexBuffer.Vertices[I] := TPoint3D.Zero;
        VertexBuffer.Normals[I] := TPoint3D.Zero;
        VertexBuffer.TexCoord0[I] := TPointF.Zero;
        VertexBuffer.Vertices[Length(Points) + I] := TPoint3D.Zero;
        VertexBuffer.Normals[Length(Points) + I] := TPoint3D.Zero;
        VertexBuffer.TexCoord0[Length(Points) + I] := TPointF.Zero;
        Continue;
      end;
      if (I > 0) then
      begin
        if Points[I - 1].X >= $FFFF then
        begin
          if (I > 1) then
            CurPos := CurPos + (Points[I] - Points[I - 2]).Length;
        end
        else
          CurPos := CurPos + (Points[I] - Points[I - 1]).Length;
      end;
      RelativePoint := TPoint3D.Create((Points[I].X - MaxLimit.X) / Abs(MinLimit.X - MaxLimit.X),
        ((Points[I].Y - MaxLimit.Y) / Abs(MinLimit.Y - MaxLimit.Y)), 0);

      VertexBuffer.Vertices[I] := TPoint3D.Create(Center.X - (Size.X / 2) + (RelativePoint.X * Size.X),
        Center.Y - (Size.Y / 2) + (RelativePoint.Y * Size.Y), Center.Z - (Size.Z / 2) + 1 * Size.Z);
      VertexBuffer.TexCoord0[I] := TPointF.Create(0, CurPos / LeftLen);

      VertexBuffer.Vertices[Length(Points) + I] := TPoint3D.Create(Center.X - (Size.X / 2) + (RelativePoint.X * Size.X),
        Center.Y - (Size.Y / 2) + (RelativePoint.Y * Size.Y), Center.Z - (Size.Z / 2) + 0 * Size.Z);
      VertexBuffer.TexCoord0[Length(Points) + I] := TPointF.Create(1, CurPos / LeftLen);
    end;
    // set indices
    IndexBuffer.Length := High(Points) * 6;
    J := 0;
    for I := 0 to High(Points) - 1 do
    begin
      if (Points[I].X >= $FFFF) and (Points[I].Y >= $FFFF) then
        Continue;
      if (Points[I + 1].X >= $FFFF) and (Points[I + 1].X >= $FFFF) then
        Continue;

      IndexBuffer[(J * 6) + 0] := I;
      IndexBuffer[(J * 6) + 2] := Length(Points) + I;
      IndexBuffer[(J * 6) + 1] := Length(Points) + I + 1;
      IndexBuffer[(J * 6) + 3] := Length(Points) + I + 1;
      IndexBuffer[(J * 6) + 5] := I + 1;
      IndexBuffer[(J * 6) + 4] := I;
      Inc(J);
    end;
    IndexBuffer.Length := J * 6;
    // Calculate face normals
    for I := 0 to (IndexBuffer.Length div 6) - 1 do
    begin
      Index := I * 6;

      VertexIndex1 := IndexBuffer[Index];
      VertexIndex2 := IndexBuffer[Index + 2];
      VertexIndex3 := IndexBuffer[Index + 1];

      PrevVertexIndex1 := IndexBuffer[(Index + IndexBuffer.Length - 6) mod IndexBuffer.Length];
      PrevVertexIndex2 := IndexBuffer[(Index + 2 + IndexBuffer.Length - 6) mod IndexBuffer.Length];
      PrevVertexIndex3 := IndexBuffer[(Index + 1 + IndexBuffer.Length - 6) mod IndexBuffer.Length];

      Vertex1 := VertexBuffer.Vertices[VertexIndex1];
      Vertex2 := VertexBuffer.Vertices[VertexIndex2];
      Vertex3 := VertexBuffer.Vertices[VertexIndex3];
      FaceVector1 := Vertex3 - Vertex1;
      FaceVector2 := Vertex3 - Vertex2;

      Vertex1 := VertexBuffer.Vertices[PrevVertexIndex1];
      Vertex2 := VertexBuffer.Vertices[PrevVertexIndex2];
      Vertex3 := VertexBuffer.Vertices[PrevVertexIndex3];
      PrevFaceVector1 := Vertex3 - Vertex1;
      PrevFaceVector2 := Vertex3 - Vertex2;

      PreviousNormal := PrevFaceVector1.CrossProduct(PrevFaceVector2).Normalize;
      CurrentNormal := FaceVector1.CrossProduct(FaceVector2).Normalize;

      Normal := (PreviousNormal + CurrentNormal).Normalize;

      VertexBuffer.Normals[VertexIndex1] := Normal;
      VertexBuffer.Normals[VertexIndex2] := Normal;
    end;
    VertexBuffer.Normals[(VertexBuffer.Length div 2) - 1] := VertexBuffer.Normals[0];
    VertexBuffer.Normals[VertexBuffer.Length - 1] := VertexBuffer.Normals[VertexBuffer.Length div 2];

    { draw }
    DrawTriangles(VertexBuffer, IndexBuffer, Material, Opacity);
  end;
  { free }
  VertexBuffer.Free;
  IndexBuffer.Free;
end;

procedure TContext3D.FillRect(const TopLeft, BottomRight: TPoint3D; const Opacity: Single; const Color: TAlphaColor);
var
  Ver: TVertexBuffer;
  Idx: TIndexBuffer;
  Mat: TColorMaterial;
  Offset: Single;
  I: Integer;
begin
  Idx := nil;
  Ver := nil;
  Mat := nil;
  try
    Ver := TVertexBuffer.Create([TVertexFormat.Vertex], 6);
    if FCurrentStates[TContextState.cs2DScene] then
      Offset := 0.5
    else
      Offset := 0.0;
    Ver.Vertices[0] := TPoint3D.Create(TopLeft.X - Offset, TopLeft.Y - Offset, TopLeft.Z);
    Ver.Vertices[1] := TPoint3D.Create(BottomRight.X + Offset, TopLeft.Y - Offset, TopLeft.Z);
    Ver.Vertices[2] := TPoint3D.Create(BottomRight.X + Offset, BottomRight.Y + Offset, TopLeft.Z);
    Ver.Vertices[3] := TPoint3D.Create(BottomRight.X + Offset, BottomRight.Y + Offset, TopLeft.Z);
    Ver.Vertices[4] := TPoint3D.Create(TopLeft.X - Offset, BottomRight.Y + Offset, TopLeft.Z);
    Ver.Vertices[5] := TPoint3D.Create(TopLeft.X - Offset, TopLeft.Y - Offset, TopLeft.Z);
    Idx := TIndexBuffer.Create(6);
    for I := 0 to Ver.Length - 1 do
      Idx[I] := I;
    Mat := TColorMaterial.Create;
    Mat.Color := Color;
    DrawTriangles(Ver, Idx, Mat, Opacity);
  finally
    Idx.Free;
    Ver.Free;
    Mat.Free;
  end;
end;

class procedure TContext3D.FinalizeShader(const Shader: TContextShader);
begin
  if Shader <> nil then
    DoFinalizeShader(Shader);
end;

{ textures }

class procedure TContext3D.InitializeTexture(const Texture: TTexture);
begin
  if (Texture <> nil) and (Texture.Handle = 0) and not (Texture.IsEmpty) then
  begin
    if Texture.PixelFormat = TPixelFormat.None then
      Texture.PixelFormat := PixelFormat;
    DoInitializeTexture(Texture);
  end;
end;

class function TContext3D.MaxLightCount: Integer;
begin
  Result := DefaultMaxLightCount;
end;

class procedure TContext3D.FinalizeTexture(const Texture: TTexture);
begin
  if (Texture <> nil) and (Texture.Handle <> 0) then
    DoFinalizeTexture(Texture);
  if Texture <> nil then
    Texture.FHandle := 0;
end;

class procedure TContext3D.UpdateTexture(const Texture: TTexture; const Bits: Pointer; const Pitch: Integer);
begin
  if Texture <> nil then
  begin
    if (Texture.Handle = 0) then
      InitializeTexture(Texture);
    DoUpdateTexture(Texture, Bits, Pitch);
  end;
end;

procedure TContext3D.DoCopyToBitmap(const Dest: TBitmap; const ARect: TRect);
var
  M: TBitmapData;
  I: Integer;
begin
  if (Dest <> nil) and Dest.Map(TMapAccess.Write, M) then
  try
    if (ARect.Width > Width) or (ARect.Height > Height) then
    begin
      for I := ARect.Top to ARect.Bottom - 1 do
        FillChar(M.GetPixelAddr(ARect.Left, I)^, ARect.Width * M.BytesPerPixel, 0);
      ARect.Intersect(Rect(0, 0, Width, Height));
    end;
    DoCopyToBits(M.Data, M.Pitch, ARect);
    if (PixelFormat <> Dest.PixelFormat) and (PixelFormatBytes[PixelFormat] = PixelFormatBytes[Dest.PixelFormat]) then
      ChangePixelFormat(M.Data, M.Data, M.Width * M.Height, PixelFormat, Dest.PixelFormat);
  finally
    Dest.Unmap(M);
  end;
end;

procedure TContext3D.CopyToBitmap(const Dest: TBitmap; const ARect: TRect);
begin
  DoCopyToBitmap(Dest, ARect);
end;

procedure TContext3D.CopyToBits(const Bits: Pointer; const Pitch: Integer; const ARect: TRect);
begin
  DoCopyToBits(Bits, Pitch, ARect);
end;

function TContext3D.GetIndexBufferSupport: TIndexBufferSupport;
begin
  Result := TIndexBufferSupport.Int32;
end;

procedure TContext3D.DrawPrimitivesMultiBatch(const AKind: TPrimitivesKind; const Vertices, Indices: Pointer;
  const VertexDeclaration: TVertexDeclaration; const VertexSize, VertexCount, IndexSize, IndexCount: Integer);
var
  VertexBuffer: Pointer;
  VertexBufferCount: Integer;
  IndexBuffer: Pointer;
  IndexBufferCount: Integer;
  VertexMap: TDictionary<Integer, Integer>;

  function InsertVertex(const SourceVertexIndex: Integer): Integer;
  begin
    Result := VertexBufferCount;
    Move(Pointer(NativeInt(Vertices) + SourceVertexIndex * VertexSize)^, Pointer(NativeInt(VertexBuffer) +
      VertexBufferCount * VertexSize)^, VertexSize);
    Inc(VertexBufferCount);
  end;

  function RemapVertex(const SourceIndex: Integer): Integer;
  begin
    if not VertexMap.TryGetValue(SourceIndex, Result) then
    begin
      Result := InsertVertex(SourceIndex);
      VertexMap.Add(SourceIndex, Result);
    end;
  end;

  procedure FlushBuffer;
  begin
    DoDrawPrimitivesBatch(AKind, VertexBuffer, IndexBuffer, VertexDeclaration, VertexSize, VertexBufferCount,
      SizeOf(Word), IndexBufferCount);
    VertexBufferCount := 0;
    IndexBufferCount := 0;
    VertexMap.Clear;
  end;

var
  I, J, IndicesPerPrimitive: Integer;
begin
  case AKind of
    TPrimitivesKind.Points:
      IndicesPerPrimitive := 1;
    TPrimitivesKind.Lines:
      IndicesPerPrimitive := 2;
  else
    IndicesPerPrimitive := 3;
  end;

  GetMem(VertexBuffer, VertexSize * MaxInt16Vertices);
  GetMem(IndexBuffer, SizeOf(Word) * MaxInt16Indices);
  VertexMap := TDictionary<Integer, Integer>.Create;
  try
    VertexBufferCount := 0;
    IndexBufferCount := 0;

    for J := 0 to (IndexCount div IndicesPerPrimitive) - 1 do
    begin
      for I := 0 to IndicesPerPrimitive - 1 do
      begin
        PWord(NativeInt(IndexBuffer) + IndexBufferCount * SizeOf(Word))^ := RemapVertex(
          PInteger(NativeInt(Indices) + ((J * IndicesPerPrimitive) + I) * SizeOf(Integer))^);
        Inc(IndexBufferCount);
      end;
      if (VertexBufferCount >= MaxInt16Vertices - (IndicesPerPrimitive - 1)) or
        (IndexBufferCount >= MaxInt16Indices - (IndicesPerPrimitive - 1)) then
        FlushBuffer;
    end;

    if IndexBufferCount > 0 then
      FlushBuffer;
  finally
    VertexMap.Free;
    FreeMem(IndexBuffer);
    FreeMem(VertexBuffer);
  end;
end;

procedure TContext3D.DrawRect(const TopLeft, BottomRight: TPoint3D; const Opacity: Single; const Color: TAlphaColor);
var
  Ver: TVertexBuffer;
  Idx: TIndexBuffer;
  Mat: TColorMaterial;
  Offset: Single;
  I: Integer;
begin
  Idx := nil;
  Ver := nil;
  Mat := nil;
  try
    Ver := TVertexBuffer.Create([TVertexFormat.Vertex], 8);
    if FCurrentStates[TContextState.cs2DScene] then
      Offset := 0.5
    else
      Offset := 0.0;
    Ver.Vertices[0] := TPoint3D.Create(TopLeft.X - Offset, TopLeft.Y - Offset, TopLeft.Z);
    Ver.Vertices[1] := TPoint3D.Create(BottomRight.X + Offset, TopLeft.Y - Offset, TopLeft.Z);
    Ver.Vertices[2] := TPoint3D.Create(BottomRight.X + Offset, TopLeft.Y - Offset, TopLeft.Z);
    Ver.Vertices[3] := TPoint3D.Create(BottomRight.X + Offset, BottomRight.Y + Offset, TopLeft.Z);
    Ver.Vertices[4] := TPoint3D.Create(BottomRight.X + Offset, BottomRight.Y + Offset, TopLeft.Z);
    Ver.Vertices[5] := TPoint3D.Create(TopLeft.X - Offset, BottomRight.Y + Offset, TopLeft.Z);
    Ver.Vertices[6] := TPoint3D.Create(TopLeft.X - Offset, BottomRight.Y + Offset, TopLeft.Z);
    Ver.Vertices[7] := TPoint3D.Create(TopLeft.X - Offset, TopLeft.Y - Offset, TopLeft.Z);
    Idx := TIndexBuffer.Create(8);
    for I := 0 to Ver.Length - 1 do
      Idx[I] := I;
    Mat := TColorMaterial.Create;
    Mat.Color := Color;
    DrawLines(Ver, Idx, Mat, Opacity);
  finally
    Idx.Free;
    Ver.Free;
    Mat.Free;
  end;
end;

procedure TContext3D.DoDrawPrimitives(const AKind: TPrimitivesKind; const Vertices, Indices: Pointer;
  const VertexDeclaration: TVertexDeclaration; const VertexSize, VertexCount, IndexSize, IndexCount: Integer);
begin
  if (IndexBufferSupport = TIndexBufferSupport.Int16) and (IndexSize > SizeOf(Word)) and
    ((VertexCount >= MaxInt16Vertices) or (IndexCount >= MaxInt16Indices)) then
    DrawPrimitivesMultiBatch(AKind, Vertices, Indices, VertexDeclaration, VertexSize, VertexCount, IndexSize,
      IndexCount)
  else
    DoDrawPrimitivesBatch(AKind, Vertices, Indices, VertexDeclaration, VertexSize, VertexCount, IndexSize,
      IndexCount);
end;

{ TNullContext }

type
  TNullContext = class(TContext3D)
  protected
    { buffer }
    procedure DoCreateBuffer; override;
    procedure DoResize; override;
    procedure DoFreeBuffer; override;
    procedure DoCopyToBits(const Bits: Pointer; const Pitch: Integer; const ARect: TRect); override;
    { scene }
    function DoBeginScene: Boolean; override;
    procedure DoEndScene; override;
    { states }
    procedure DoClear(const ATarget: TClearTargets; const AColor: TAlphaColor; const ADepth: single; const AStencil: Cardinal); override;
    procedure DoSetContextState(AState: TContextState); override;
    procedure DoSetStencilOp(const Fail, ZFail, ZPass: TStencilOp); override;
    procedure DoSetStencilFunc(const Func: TStencilfunc; Ref, Mask: Cardinal); override;
    { drawing }
    procedure DoDrawPrimitivesBatch(const AKind: TPrimitivesKind; const Vertices, Indices: Pointer;
      const VertexDeclaration: TVertexDeclaration; const VertexSize, VertexCount, IndexSize,
      IndexCount: Integer); override;
    { texture }
    class procedure DoInitializeTexture(const Texture: TTexture); override;
    class procedure DoFinalizeTexture(const Texture: TTexture); override;
    class procedure DoUpdateTexture(const Texture: TTexture; const Bits: Pointer; const Pitch: Integer); override;
    { shaders }
    class procedure DoInitializeShader(const Shader: TContextShader); override;
    class procedure DoFinalizeShader(const Shader: TContextShader); override;
    procedure DoSetShaders(const VertexShader, PixelShader: TContextShader); override;
    procedure DoSetShaderVariable(const Name: string; const Data: array of TVector3D); override;
    procedure DoSetShaderVariable(const Name: string; const Texture: TTexture); override;
    procedure DoSetShaderVariable(const Name: string; const Matrix: TMatrix3D); override;
  public
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AMultisample: TMultisample; const ADepthStencil: Boolean); override;
    constructor CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample;
      const ADepthStencil: Boolean); override;
    class function Valid: Boolean; override;
  end;

{ TCustomDX9Context }

constructor TNullContext.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
  const AMultisample: TMultisample; const ADepthStencil: Boolean);
begin
  inherited;
  CreateBuffer;
end;

constructor TNullContext.CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample; const ADepthStencil: Boolean);
begin
  inherited;
  CreateBuffer;
end;

class function TNullContext.Valid: Boolean;
begin
  Result := False;
end;

procedure TNullContext.DoCreateBuffer;
begin
end;

procedure TNullContext.DoDrawPrimitivesBatch(const AKind: TPrimitivesKind; const Vertices, Indices: Pointer;
  const VertexDeclaration: TVertexDeclaration; const VertexSize, VertexCount, IndexSize, IndexCount: Integer);
begin
end;

procedure TNullContext.DoResize;
begin
end;

procedure TNullContext.DoFreeBuffer;
begin
end;

procedure TNullContext.DoClear(const ATarget: TClearTargets; const AColor: TAlphaColor; const ADepth: single; const AStencil: Cardinal);
begin
end;

procedure TNullContext.DoCopyToBits(const Bits: Pointer; const Pitch: Integer;
  const ARect: TRect);
begin

end;

function TNullContext.DoBeginScene: Boolean;
begin
  Result := False;
end;

procedure TNullContext.DoEndScene;
begin
end;

procedure TNullContext.DoSetContextState(AState: TContextState);
begin
end;

procedure TNullContext.DoSetStencilOp(const Fail, ZFail, ZPass: TStencilOp);
begin
end;

procedure TNullContext.DoSetStencilFunc(const Func: TStencilfunc; Ref, Mask: Cardinal);
begin
end;

class procedure TNullContext.DoInitializeTexture(const Texture: TTexture);
begin
end;

class procedure TNullContext.DoFinalizeTexture(const Texture: TTexture);
begin
end;

class procedure TNullContext.DoUpdateTexture(const Texture: TTexture; const Bits: Pointer; const Pitch: Integer);
begin
end;

class procedure TNullContext.DoInitializeShader(const Shader: TContextShader);
begin
end;

class procedure TNullContext.DoFinalizeShader(const Shader: TContextShader);
begin
end;

procedure TNullContext.DoSetShaders(const VertexShader, PixelShader: TContextShader);
begin
end;

procedure TNullContext.DoSetShaderVariable(const Name: string; const Data: array of TVector3D);
begin
end;

procedure TNullContext.DoSetShaderVariable(const Name: string; const Texture: TTexture);
begin
end;

procedure TNullContext.DoSetShaderVariable(const Name: string; const Matrix: TMatrix3D);
begin
end;

{ TContextManager }

class function TContextManager.CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample;
  const ADepthStencil: Boolean): TContext3D;
begin
  if DefaultContextClass <> nil then
    Result := DefaultContextClass.CreateFromTexture(ATexture, AMultisample, ADepthStencil)
  else
    Result := nil;
end;

class function TContextManager.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer; const AMultisample: TMultisample;
  const ADepthStencil: Boolean): TContext3D;
begin
  if DefaultContextClass <> nil then
    Result := DefaultContextClass.CreateFromWindow(AParent, AWidth, AHeight, AMultisample, ADepthStencil)
  else
    Result := nil;
end;

class function TContextManager.GetContextCount: Integer;
begin
  if FContextList <> nil then
    Result := FContextList.Count
  else
    Result := 0;
end;

class function TContextManager.GetDefaultContextClass: TContextClass;
var
  ContextSrv: IFMXContextService;
  ContextClassRec: TContextClassRec;
begin
  if FDefaultContextClass = nil then
  begin
    if (TPlatformServices.Current <> nil) and TPlatformServices.Current.SupportsPlatformService(IFMXContextService, ContextSrv) then
      ContextSrv.RegisterContextClasses;
    if (FContextList <> nil) and (FContextList.Count > 0) then
    begin
      for ContextClassRec in FContextList do
        if ContextClassRec.Default then
        begin
          FDefaultContextClass := ContextClassRec.ContextClass;
          Break;
        end;
      if FDefaultContextClass = nil then
        FDefaultContextClass := FContextList[0].ContextClass;
    end
    else
      FDefaultContextClass := TNullContext;
  end;
  Result := FDefaultContextClass;
end;

class procedure TContextManager.InitializeShader(const Shader: TContextShader);
begin
  DefaultContextClass.InitializeShader(Shader);
end;

class procedure TContextManager.FinalizeShader(const Shader: TContextShader);
begin
  DefaultContextClass.FinalizeShader(Shader);
end;

class procedure TContextManager.RegisterContext(const ContextClass: TContextClass; const ADefault: Boolean);
var
  Rec: TContextClassRec;
begin
  if FContextList = nil then
    FContextList := TList<TContextClassRec>.Create;
  Rec.ContextClass := ContextClass;
  Rec.Default := ADefault;
  FContextList.Add(Rec);
end;

class procedure TContextManager.UnInitialize;
var
  ContextSrv: IFMXContextService;
begin
  if FContextList <> nil then
  begin
    FreeAndNil(FContextList);
    if (TPlatformServices.Current <> nil) and TPlatformServices.Current.SupportsPlatformService(IFMXContextService, ContextSrv) then
      ContextSrv.UnregisterContextClasses;
  end;
  FDefaultContextClass := nil;
end;

{ TPosition3D }

constructor TPosition3D.Create(const ADefaultValue: TPoint3D);
begin
  inherited Create;
  FDefaultValue := ADefaultValue;
  FX := FDefaultValue.X;
  FY := FDefaultValue.Y;
  FZ := FDefaultValue.Z;
end;

procedure TPosition3D.Assign(Source: TPersistent);
begin
  if Source is TPosition3D then
  begin
    Point := TPosition3D(Source).Point;
  end
  else
    inherited
end;

procedure TPosition3D.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Point', ReadPoint, WritePoint, False);
end;

procedure TPosition3D.ReadPoint(Reader: TReader);
begin
  Point := StringToPoint3D(Reader.ReadString);
end;

procedure TPosition3D.WritePoint(Writer: TWriter);
begin
  Writer.WriteString(String(Point3DToString(Point)));
end;

function TPosition3D.GetVector: TVector3D;
begin
  Result := Vector3D(FX, FY, FZ);
end;

function TPosition3D.IsXStored: Boolean;
begin
  Result := FX <> FDefaultValue.X;
end;

function TPosition3D.IsYStored: Boolean;
begin
  Result := FY <> FDefaultValue.Y;
end;

function TPosition3D.IsZStored: Boolean;
begin
  Result := FZ <> FDefaultValue.Z;
end;

procedure TPosition3D.SetVector(const Value: TVector3D);
begin
  SetPoint3D(TPoint3D(Value));
end;

function TPosition3D.GetPoint3D: TPoint3D;
begin
  Result := TPoint3D.Create(FX, FY, FZ);
end;

procedure TPosition3D.SetVectorNoChange(const P: TVector3D);
begin
  FX := P.X;
  FY := P.Y;
  FZ := P.Z;
end;

procedure TPosition3D.SetPoint3DNoChange(const P: TPoint3D);
begin
  FX := P.X;
  FY := P.Y;
  FZ := P.Z;
end;

procedure TPosition3D.SetPoint3D(const Value: TPoint3D);
begin
  FX := Value.X;
  FY := Value.Y;
  FZ := Value.Z;
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TPosition3D.SetX(const Value: Single);
begin
  if FX <> Value then
  begin
    FX := Value;
    if Assigned(OnChangeX) then
      OnChangeX(Self)
    else if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TPosition3D.SetY(const Value: Single);
begin
  if FY <> Value then
  begin
    FY := Value;
    if Assigned(OnChangeY) then
      OnChangeY(Self)
    else if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TPosition3D.SetZ(const Value: Single);
begin
  if FZ <> Value then
  begin
    FZ := Value;
    if Assigned(OnChangeZ) then
      OnChangeZ(Self)
    else if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

function TPosition3D.Empty: Boolean;
begin
  Result := (FX = 0) and (FY = 0) and (FZ = 0);
end;

{ TBoundingBox }

constructor TBoundingBox.Create(const ABox: TBoundingBox; NormalizeBox: Boolean);
begin
  Self := ABox;
  if NormalizeBox then
    Self := Normalize;
end;

constructor TBoundingBox.Create(const AnOrigin: TPoint3D);
begin
  TopLeftNear := AnOrigin;
  BottomRightFar := AnOrigin;
end;

constructor TBoundingBox.Create(const Left, Top, Near, Right, Bottom, Far: Single);
begin
  Self.Left := Left;
  Self.Top := Top;
  Self.Near := Near;
  Self.Right := Right;
  Self.Bottom := Bottom;
  Self.Far := Far;
end;

constructor TBoundingBox.Create(const APoint1, APoint2: TPoint3D; NormalizeBox: Boolean);
begin
  TopLeftNear := APoint1;
  BottomRightFar := APoint2;
  if NormalizeBox then
    Self := Normalize;
end;

constructor TBoundingBox.Create(const AnOrigin: TPoint3D; const Width, Height, Depth: Single);
begin
  TopLeftNear := AnOrigin;
  BottomRightFar := AnOrigin + TPoint3D.Create(Width, Height, Depth);
end;

constructor TBoundingBox.Create(const Points: PPoint3D; const PointCount: Integer);
var
  I: Integer;
  LMinCorner, LMaxCorner: TPoint3D;
  PointsIterator: PPoint3D;
begin
  if (PointCount > 0) and (Points <> nil) then
  begin
    LMinCorner := Points^;
    LMaxCorner := Points^;
    PointsIterator := Points;

    for I := 1 to PointCount - 1 do
    begin
      LMinCorner.X := Min(PointsIterator.X, LMinCorner.X);
      LMinCorner.Y := Min(PointsIterator.Y, LMinCorner.Y);
      LMinCorner.Z := Min(PointsIterator.Z, LMinCorner.Z);

      LMaxCorner.X := Max(PointsIterator.X, LMaxCorner.X);
      LMaxCorner.Y := Max(PointsIterator.Y, LMaxCorner.Y);
      LMaxCorner.Z := Max(PointsIterator.Z, LMaxCorner.Z);

      Inc(PointsIterator);
    end;

    Self := TBoundingBox.Create(LMinCorner, LMaxCorner);
  end
  else
  begin
    Self := TBoundingBox.Empty;
  end;
end;

constructor TBoundingBox.Create(const Points: TArray<TPoint3D>);
begin
  Self := Create(@Points[0], Length(Points))
end;

class operator TBoundingBox.Equal(const LeftBox, RightBox: TBoundingBox): Boolean;
begin
  Result := LeftBox.EqualsTo(RightBox, TEpsilon.Vector);
end;

function TBoundingBox.EqualsTo(const ABox: TBoundingBox; const Epsilon: Single): Boolean;
begin
  Result := TopLeftNear.EqualsTo(ABox.TopLeftNear, Epsilon) and BottomRightFar.EqualsTo(ABox.BottomRightFar, Epsilon);
end;

function TBoundingBox.FitIntoScale(const ADesignatedArea: TBoundingBox): Single;
var
  LWidthFactor, LHeightFactor, LDepthFactor: Single;
begin
  if (ADesignatedArea.Width <= 0) or (ADesignatedArea.Height <= 0) or (ADesignatedArea.Depth <= 0) then
    Result := 1
  else
  begin
    LWidthFactor := Width / ADesignatedArea.Width;
    LHeightFactor := Height / ADesignatedArea.Height;
    LDepthFactor := Depth / ADesignatedArea.Depth;

    if LWidthFactor > LHeightFactor then
    begin
      if LWidthFactor > LDepthFactor then
        Result := LWidthFactor
      else
        Result := LDepthFactor;
    end
    else
    begin
      if LHeightFactor > LDepthFactor then
        Result := LHeightFactor
      else
        Result := LDepthFactor;
    end;
  end;
end;

function TBoundingBox.FitInto(const ADesignatedArea: TBoundingBox; out ARatio: Single): TBoundingBox;
  function BoxCenter(var ABox: TBoundingBox; const Bounds: TBoundingBox): TBoundingBox;
  var
    MidPoint, Offset: TPoint3D;
  begin
    MidPoint := Bounds.CenterPoint;
    Offset.X := ABox.Width * 0.5;
    Offset.Y := ABox.Height * 0.5;
    Offset.Z := ABox.Depth * 0.5;

    ABox.MinCorner := MidPoint - Offset;
    ABox.MaxCorner := MidPoint + Offset;
    Result := ABox;
  end;
var
  LInvRatio: Single;
begin
  if (ADesignatedArea.Width <= 0) or (ADesignatedArea.Height <= 0) or (ADesignatedArea.Depth <= 0) then
  begin
    ARatio := 1;
    Result := Self;
  end
  else
  begin
    ARatio := FitIntoScale(ADesignatedArea);
    LInvRatio := 1 / ARatio;

    Result := TBoundingBox.Create(0, 0, 0, Width * LInvRatio, Height * LInvRatio, Depth * LInvRatio);
    BoxCenter(Result, ADesignatedArea);
  end;
end;

function TBoundingBox.FitInto(const ADesignatedArea: TBoundingBox): TBoundingBox;
var
  Ratio: Single;
begin
  Result := FitInto(ADesignatedArea, Ratio);
end;

class operator TBoundingBox.NotEqual(const LeftBox, RightBox: TBoundingBox): Boolean;
begin
  Result := not LeftBox.EqualsTo(RightBox, TEpsilon.Vector);
end;

class operator TBoundingBox.Add(const LeftBox, RightBox: TBoundingBox): TBoundingBox;
begin
  Result := LeftBox.Union(RightBox);
end;

class operator TBoundingBox.Multiply(const LeftBox, RightBox: TBoundingBox): TBoundingBox;
begin
  Result := LeftBox.Intersect(RightBox);
end;

function TBoundingBox.GetCenterPoint: TPoint3D;
begin
{$EXCESSPRECISION OFF}
  Result.X := (Right + Left) * 0.5;
  Result.Y := (Bottom + Top) * 0.5;
  Result.Z := (Near + Far) * 0.5;
{$EXCESSPRECISION ON}
end;

function TBoundingBox.Contains(const ABox: TBoundingBox): Boolean;
begin
  Result := Contains(ABox.TopLeftNear) and Contains(ABox.BottomRightFar);
end;

function IsPointInBox(const Box: TBoundingBox; const P: TPoint3D): Boolean;
begin
  Result := (P.X >= Box.Left) and (P.X < Box.Right) and (P.Y >= Box.Top) and (P.Y < Box.Bottom) and (P.Z >= Box.Near)
    and (P.Z < Box.Far);
end;

function TBoundingBox.Contains(const APoint: TPoint3D): Boolean;
begin
  Result := ((APoint.X > Left) or SameValue(APoint.X, Left, TEpsilon.Vector)) and (APoint.X < Right) and
    ((APoint.Y > Top) or SameValue(APoint.Y, Top, TEpsilon.Vector)) and (APoint.Y < Bottom) and
    ((APoint.Z > Near) or SameValue(APoint.Z, Near, TEpsilon.Vector)) and (APoint.Z < Far);
end;

class function TBoundingBox.Empty: TBoundingBox;
begin
  Result := TBoundingBox.Create(0, 0, 0, 0, 0, 0);
end;

function TBoundingBox.GetHeight: Single;
begin
{$EXCESSPRECISION OFF}
  Result := Bottom - Top;
{$EXCESSPRECISION ON}
end;

procedure TBoundingBox.SetHeight(const Value: Single);
begin
{$EXCESSPRECISION OFF}
  Bottom := Top + Value;
{$EXCESSPRECISION ON}
end;

function TBoundingBox.GetDepth: Single;
begin
{$EXCESSPRECISION OFF}
  Result := Far - Near;
{$EXCESSPRECISION ON}
end;

function TBoundingBox.GetSize: TPoint3D;
begin
{$EXCESSPRECISION OFF}
  Result.X := Right - Left;
  Result.Y := Bottom - Top;
  Result.Z := Far - Near;
{$EXCESSPRECISION ON}
end;

procedure TBoundingBox.SetDepth(const Value: Single);
begin
{$EXCESSPRECISION OFF}
  Far := Near + Value;
{$EXCESSPRECISION ON}
end;
function TBoundingBox.GetWidth: Single;
begin
{$EXCESSPRECISION OFF}
  Result := Right - Left;
{$EXCESSPRECISION ON}
end;

procedure TBoundingBox.SetWidth(const Value: Single);
begin
{$EXCESSPRECISION OFF}
  Right := Left + Value;
{$EXCESSPRECISION ON}
end;

function TBoundingBox.Inflate(const DX, DY, DZ: Single): TBoundingBox;
begin
{$EXCESSPRECISION OFF}
  Result.TopLeftNear := TopLeftNear + TPoint3D.Create(-DX, -DY, -DZ);
  Result.BottomRightFar := BottomRightFar + TPoint3D.Create(DX, DY, DZ);
{$EXCESSPRECISION ON}
end;

function TBoundingBox.Inflate(const DL, DT, DN, DR, DB, DF: Single): TBoundingBox;
begin
{$EXCESSPRECISION OFF}
  Result.TopLeftNear := TopLeftNear + TPoint3D.Create(-DL, -DT, -DN);
  Result.BottomRightFar := BottomRightFar + TPoint3D.Create(DR, DB, DF);
{$EXCESSPRECISION ON}
end;

function TBoundingBox.Offset(const APoint: TPoint3D): TBoundingBox;
begin
  Result.TopLeftNear := TopLeftNear + APoint;
  Result.BottomRightFar := BottomRightFar + APoint;
end;

function TBoundingBox.Offset(const DX, DY, DZ: Single): TBoundingBox;
begin
  Result.TopLeftNear := TopLeftNear + TPoint3D.Create(DX, DY, DZ);
  Result.BottomRightFar := BottomRightFar + TPoint3D.Create(DX, DY, DZ);
end;

function TBoundingBox.IntersectsWith(const ABox: TBoundingBox): Boolean;
begin
  Result := (MaxCorner.X >= ABox.MinCorner.X) and (MaxCorner.Y >= ABox.MinCorner.Y) and
    (MaxCorner.Z >= ABox.MinCorner.Z) and (ABox.MaxCorner.X >= MinCorner.X) and (ABox.MaxCorner.Y >= MinCorner.Y) and
    (ABox.MaxCorner.Z >= MinCorner.Z);
end;

function TBoundingBox.IsEmpty(const Epsilon: Single): Boolean;
begin
  Result := (Right < Left) or SameValue(Right, Left, Epsilon) or (Bottom < Top) or
    SameValue(Bottom, Top, Epsilon) or (Far < Near) or SameValue(Far, Near, Epsilon);
end;

function TBoundingBox.Normalize: TBoundingBox;
begin
  Result.Top := Min(Bottom, Top);
  Result.Bottom := Max(Bottom, Top);

  Result.Left := Min(Left, Right);
  Result.Right := Max(Left, Right);

  Result.Near := Min(Near, Far);
  Result.Far := Max(Near, Far);
end;

function IsBoxEmpty(const ABox: TBoundingBox): Boolean;
begin
  Result := (ABox.Right <= ABox.Left) or (ABox.Bottom <= ABox.Top) or (ABox.Far <= ABox.Near);
end;

function TBoundingBox.Intersect(const DestBox: TBoundingBox): TBoundingBox;
begin
  Result.MinCorner.X := Max(MinCorner.X, DestBox.MinCorner.X);
  Result.MaxCorner.X := Min(MaxCorner.X, DestBox.MaxCorner.X);

  Result.MinCorner.Y := Max(MinCorner.Y, DestBox.MinCorner.Y);
  Result.MaxCorner.Y := Min(MaxCorner.Y, DestBox.MaxCorner.Y);

  Result.MinCorner.Z := Max(MinCorner.Z, DestBox.MinCorner.Z);
  Result.MaxCorner.Z := Min(MaxCorner.Z, DestBox.MaxCorner.Z);
end;

function TBoundingBox.Union(const DestBox: TBoundingBox): TBoundingBox;
begin
  if DestBox.IsEmpty then
    Result := Self
  else
    if IsEmpty then
      Result := DestBox
    else
    begin
      Result.MinCorner.X := Min(MinCorner.X, DestBox.MinCorner.X);
      Result.MaxCorner.X := Max(MaxCorner.X, DestBox.MaxCorner.X);

      Result.MinCorner.Y := Min(MinCorner.Y, DestBox.MinCorner.Y);
      Result.MaxCorner.Y := Max(MaxCorner.Y, DestBox.MaxCorner.Y);

      Result.MinCorner.Z := Min(MinCorner.Z, DestBox.MinCorner.Z);
      Result.MaxCorner.Z := Max(MaxCorner.Z, DestBox.MaxCorner.Z);
    end;
end;

initialization
  RegisterFmxClasses([TMeshData], [TMeshData]);
end.
