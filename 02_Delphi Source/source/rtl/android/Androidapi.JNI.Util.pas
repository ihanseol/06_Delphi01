{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Util;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes;

type
// ===== Forward declarations =====

  JAndroidException = interface;//android.util.AndroidException
  JAndroidRuntimeException = interface;//android.util.AndroidRuntimeException
  Jutil_ArrayMap = interface;//android.util.ArrayMap
  JAttributeSet = interface;//android.util.AttributeSet
  JDisplayMetrics = interface;//android.util.DisplayMetrics
  Jutil_Log = interface;//android.util.Log
  JLongSparseArray = interface;//android.util.LongSparseArray
  JPair = interface;//android.util.Pair
  JPrinter = interface;//android.util.Printer
  JProperty = interface;//android.util.Property
  JRange = interface;//android.util.Range
  JRational = interface;//android.util.Rational
  Jutil_Size = interface;//android.util.Size
  JSizeF = interface;//android.util.SizeF
  JSparseArray = interface;//android.util.SparseArray
  JSparseBooleanArray = interface;//android.util.SparseBooleanArray
  JTypedValue = interface;//android.util.TypedValue

// ===== Interface declarations =====

  JAndroidExceptionClass = interface(JExceptionClass)
    ['{316A42C1-5EB3-4252-BD9B-DF12B2FDF470}']
    {class} function init: JAndroidException; cdecl; overload;
    {class} function init(name: JString): JAndroidException; cdecl; overload;
    {class} function init(name: JString; cause: JThrowable): JAndroidException; cdecl; overload;
    {class} function init(cause: JException): JAndroidException; cdecl; overload;
  end;

  [JavaSignature('android/util/AndroidException')]
  JAndroidException = interface(JException)
    ['{9DD82875-9F43-483A-83BC-980F36CBCA5A}']
  end;
  TJAndroidException = class(TJavaGenericImport<JAndroidExceptionClass, JAndroidException>) end;

  JAndroidRuntimeExceptionClass = interface(JRuntimeExceptionClass)
    ['{0858B0B4-AAB9-4F0B-850F-33B575ACFE7B}']
    {class} function init: JAndroidRuntimeException; cdecl; overload;
    {class} function init(name: JString): JAndroidRuntimeException; cdecl; overload;
    {class} function init(name: JString; cause: JThrowable): JAndroidRuntimeException; cdecl; overload;
    {class} function init(cause: JException): JAndroidRuntimeException; cdecl; overload;
  end;

  [JavaSignature('android/util/AndroidRuntimeException')]
  JAndroidRuntimeException = interface(JRuntimeException)
    ['{83257DE1-7FFC-4373-B4B3-ED0330D090ED}']
  end;
  TJAndroidRuntimeException = class(TJavaGenericImport<JAndroidRuntimeExceptionClass, JAndroidRuntimeException>) end;

  Jutil_ArrayMapClass = interface(JObjectClass)
    ['{9FA79200-A0BA-426B-B157-D23942A51139}']
    {class} function init: Jutil_ArrayMap; cdecl; overload;
    {class} function init(capacity: Integer): Jutil_ArrayMap; cdecl; overload;
    {class} function init(map: Jutil_ArrayMap): Jutil_ArrayMap; cdecl; overload;
  end;

  [JavaSignature('android/util/ArrayMap')]
  Jutil_ArrayMap = interface(JObject)
    ['{439B45B3-68C7-4A51-9237-B32582344B99}']
    procedure clear; cdecl;
    function containsAll(collection: JCollection): Boolean; cdecl;
    function containsKey(key: JObject): Boolean; cdecl;
    function containsValue(value: JObject): Boolean; cdecl;
    procedure ensureCapacity(minimumCapacity: Integer); cdecl;
    function entrySet: JSet; cdecl;
    function equals(object_: JObject): Boolean; cdecl;
    procedure forEach(action: JBiConsumer); cdecl;
    function &get(key: JObject): JObject; cdecl;
    function hashCode: Integer; cdecl;
    function indexOfKey(key: JObject): Integer; cdecl;
    function indexOfValue(value: JObject): Integer; cdecl;
    function isEmpty: Boolean; cdecl;
    function keyAt(index: Integer): JObject; cdecl;
    function keySet: JSet; cdecl;
    function put(key: JObject; value: JObject): JObject; cdecl;
    procedure putAll(array_: Jutil_ArrayMap); cdecl; overload;
    procedure putAll(map: JMap); cdecl; overload;
    function remove(key: JObject): JObject; cdecl;
    function removeAll(collection: JCollection): Boolean; cdecl;
    function removeAt(index: Integer): JObject; cdecl;
    procedure replaceAll(function_: JBiFunction); cdecl;
    function retainAll(collection: JCollection): Boolean; cdecl;
    function setValueAt(index: Integer; value: JObject): JObject; cdecl;
    function size: Integer; cdecl;
    function toString: JString; cdecl;
    function valueAt(index: Integer): JObject; cdecl;
    function values: JCollection; cdecl;
  end;
  TJutil_ArrayMap = class(TJavaGenericImport<Jutil_ArrayMapClass, Jutil_ArrayMap>) end;

  JAttributeSetClass = interface(IJavaClass)
    ['{38053FDE-577B-41AB-9D3F-0572B876D9A1}']
  end;

  [JavaSignature('android/util/AttributeSet')]
  JAttributeSet = interface(IJavaInstance)
    ['{A68EE3B4-0C0D-4B03-A0A0-163C13A234CC}']
    function getAttributeBooleanValue(namespace: JString; attribute: JString; defaultValue: Boolean): Boolean; cdecl; overload;
    function getAttributeBooleanValue(index: Integer; defaultValue: Boolean): Boolean; cdecl; overload;
    function getAttributeCount: Integer; cdecl;
    function getAttributeFloatValue(namespace: JString; attribute: JString; defaultValue: Single): Single; cdecl; overload;
    function getAttributeFloatValue(index: Integer; defaultValue: Single): Single; cdecl; overload;
    function getAttributeIntValue(namespace: JString; attribute: JString; defaultValue: Integer): Integer; cdecl; overload;
    function getAttributeIntValue(index: Integer; defaultValue: Integer): Integer; cdecl; overload;
    function getAttributeListValue(namespace: JString; attribute: JString; options: TJavaObjectArray<JString>; defaultValue: Integer): Integer; cdecl; overload;
    function getAttributeListValue(index: Integer; options: TJavaObjectArray<JString>; defaultValue: Integer): Integer; cdecl; overload;
    function getAttributeName(index: Integer): JString; cdecl;
    function getAttributeNameResource(index: Integer): Integer; cdecl;
    function getAttributeNamespace(index: Integer): JString; cdecl;
    function getAttributeResourceValue(namespace: JString; attribute: JString; defaultValue: Integer): Integer; cdecl; overload;
    function getAttributeResourceValue(index: Integer; defaultValue: Integer): Integer; cdecl; overload;
    function getAttributeUnsignedIntValue(namespace: JString; attribute: JString; defaultValue: Integer): Integer; cdecl; overload;
    function getAttributeUnsignedIntValue(index: Integer; defaultValue: Integer): Integer; cdecl; overload;
    function getAttributeValue(index: Integer): JString; cdecl; overload;
    function getAttributeValue(namespace: JString; name: JString): JString; cdecl; overload;
    function getClassAttribute: JString; cdecl;
    function getIdAttribute: JString; cdecl;
    function getIdAttributeResourceValue(defaultValue: Integer): Integer; cdecl;
    function getPositionDescription: JString; cdecl;
    function getStyleAttribute: Integer; cdecl;
  end;
  TJAttributeSet = class(TJavaGenericImport<JAttributeSetClass, JAttributeSet>) end;

  JDisplayMetricsClass = interface(JObjectClass)
    ['{5B98B374-49AD-4739-873F-C979B3F824CD}']
    {class} function _GetDENSITY_140: Integer; cdecl;
    {class} function _GetDENSITY_180: Integer; cdecl;
    {class} function _GetDENSITY_200: Integer; cdecl;
    {class} function _GetDENSITY_220: Integer; cdecl;
    {class} function _GetDENSITY_260: Integer; cdecl;
    {class} function _GetDENSITY_280: Integer; cdecl;
    {class} function _GetDENSITY_300: Integer; cdecl;
    {class} function _GetDENSITY_340: Integer; cdecl;
    {class} function _GetDENSITY_360: Integer; cdecl;
    {class} function _GetDENSITY_400: Integer; cdecl;
    {class} function _GetDENSITY_420: Integer; cdecl;
    {class} function _GetDENSITY_440: Integer; cdecl;
    {class} function _GetDENSITY_450: Integer; cdecl;
    {class} function _GetDENSITY_560: Integer; cdecl;
    {class} function _GetDENSITY_600: Integer; cdecl;
    {class} function _GetDENSITY_DEFAULT: Integer; cdecl;
    {class} function _GetDENSITY_DEVICE_STABLE: Integer; cdecl;
    {class} function _GetDENSITY_HIGH: Integer; cdecl;
    {class} function _GetDENSITY_LOW: Integer; cdecl;
    {class} function _GetDENSITY_MEDIUM: Integer; cdecl;
    {class} function _GetDENSITY_TV: Integer; cdecl;
    {class} function _GetDENSITY_XHIGH: Integer; cdecl;
    {class} function _GetDENSITY_XXHIGH: Integer; cdecl;
    {class} function _GetDENSITY_XXXHIGH: Integer; cdecl;
    {class} function init: JDisplayMetrics; cdecl;
    {class} property DENSITY_140: Integer read _GetDENSITY_140;
    {class} property DENSITY_180: Integer read _GetDENSITY_180;
    {class} property DENSITY_200: Integer read _GetDENSITY_200;
    {class} property DENSITY_220: Integer read _GetDENSITY_220;
    {class} property DENSITY_260: Integer read _GetDENSITY_260;
    {class} property DENSITY_280: Integer read _GetDENSITY_280;
    {class} property DENSITY_300: Integer read _GetDENSITY_300;
    {class} property DENSITY_340: Integer read _GetDENSITY_340;
    {class} property DENSITY_360: Integer read _GetDENSITY_360;
    {class} property DENSITY_400: Integer read _GetDENSITY_400;
    {class} property DENSITY_420: Integer read _GetDENSITY_420;
    {class} property DENSITY_440: Integer read _GetDENSITY_440;
    {class} property DENSITY_450: Integer read _GetDENSITY_450;
    {class} property DENSITY_560: Integer read _GetDENSITY_560;
    {class} property DENSITY_600: Integer read _GetDENSITY_600;
    {class} property DENSITY_DEFAULT: Integer read _GetDENSITY_DEFAULT;
    {class} property DENSITY_DEVICE_STABLE: Integer read _GetDENSITY_DEVICE_STABLE;
    {class} property DENSITY_HIGH: Integer read _GetDENSITY_HIGH;
    {class} property DENSITY_LOW: Integer read _GetDENSITY_LOW;
    {class} property DENSITY_MEDIUM: Integer read _GetDENSITY_MEDIUM;
    {class} property DENSITY_TV: Integer read _GetDENSITY_TV;
    {class} property DENSITY_XHIGH: Integer read _GetDENSITY_XHIGH;
    {class} property DENSITY_XXHIGH: Integer read _GetDENSITY_XXHIGH;
    {class} property DENSITY_XXXHIGH: Integer read _GetDENSITY_XXXHIGH;
  end;

  [JavaSignature('android/util/DisplayMetrics')]
  JDisplayMetrics = interface(JObject)
    ['{D3EC59F1-BB9A-4820-BBB6-47A518711902}']
    function _Getdensity: Single; cdecl;
    procedure _Setdensity(Value: Single); cdecl;
    function _GetdensityDpi: Integer; cdecl;
    procedure _SetdensityDpi(Value: Integer); cdecl;
    function _GetheightPixels: Integer; cdecl;
    procedure _SetheightPixels(Value: Integer); cdecl;
    function _GetscaledDensity: Single; cdecl;
    procedure _SetscaledDensity(Value: Single); cdecl;
    function _GetwidthPixels: Integer; cdecl;
    procedure _SetwidthPixels(Value: Integer); cdecl;
    function _Getxdpi: Single; cdecl;
    procedure _Setxdpi(Value: Single); cdecl;
    function _Getydpi: Single; cdecl;
    procedure _Setydpi(Value: Single); cdecl;
    function equals(o: JObject): Boolean; cdecl; overload;
    function equals(other: JDisplayMetrics): Boolean; cdecl; overload;
    function hashCode: Integer; cdecl;
    procedure setTo(o: JDisplayMetrics); cdecl;
    procedure setToDefaults; cdecl;
    function toString: JString; cdecl;
    property density: Single read _Getdensity write _Setdensity;
    property densityDpi: Integer read _GetdensityDpi write _SetdensityDpi;
    property heightPixels: Integer read _GetheightPixels write _SetheightPixels;
    property scaledDensity: Single read _GetscaledDensity write _SetscaledDensity;
    property widthPixels: Integer read _GetwidthPixels write _SetwidthPixels;
    property xdpi: Single read _Getxdpi write _Setxdpi;
    property ydpi: Single read _Getydpi write _Setydpi;
  end;
  TJDisplayMetrics = class(TJavaGenericImport<JDisplayMetricsClass, JDisplayMetrics>) end;

  Jutil_LogClass = interface(JObjectClass)
    ['{62108FE8-1DBB-4C4F-A0C7-35D12BD116DC}']
    {class} function _GetASSERT: Integer; cdecl;
    {class} function _GetDEBUG: Integer; cdecl;
    {class} function _GetERROR: Integer; cdecl;
    {class} function _GetINFO: Integer; cdecl;
    {class} function _GetVERBOSE: Integer; cdecl;
    {class} function _GetWARN: Integer; cdecl;
    {class} function d(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function d(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function e(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function e(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function getStackTraceString(tr: JThrowable): JString; cdecl;
    {class} function i(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function i(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function isLoggable(tag: JString; level: Integer): Boolean; cdecl;
    {class} function println(priority: Integer; tag: JString; msg: JString): Integer; cdecl;
    {class} function v(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function v(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function w(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function w(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function w(tag: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function wtf(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function wtf(tag: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function wtf(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} property ASSERT: Integer read _GetASSERT;
    {class} property DEBUG: Integer read _GetDEBUG;
    {class} property ERROR: Integer read _GetERROR;
    {class} property INFO: Integer read _GetINFO;
    {class} property VERBOSE: Integer read _GetVERBOSE;
    {class} property WARN: Integer read _GetWARN;
  end;

  [JavaSignature('android/util/Log')]
  Jutil_Log = interface(JObject)
    ['{6A5EC34E-CB76-4AB0-A11D-7CCB3B40C571}']
  end;
  TJutil_Log = class(TJavaGenericImport<Jutil_LogClass, Jutil_Log>) end;

  JLongSparseArrayClass = interface(JObjectClass)
    ['{405206B3-082B-4551-B821-FB562E0B984B}']
    {class} function init: JLongSparseArray; cdecl; overload;
    {class} function init(initialCapacity: Integer): JLongSparseArray; cdecl; overload;
  end;

  [JavaSignature('android/util/LongSparseArray')]
  JLongSparseArray = interface(JObject)
    ['{D98D0F62-5FC5-4EB2-901D-FAA33F866DF5}']
    procedure append(key: Int64; value: JObject); cdecl;
    procedure clear; cdecl;
    function clone: JLongSparseArray; cdecl;
    procedure delete(key: Int64); cdecl;
    function &get(key: Int64): JObject; cdecl; overload;
    function &get(key: Int64; valueIfKeyNotFound: JObject): JObject; cdecl; overload;
    function indexOfKey(key: Int64): Integer; cdecl;
    function indexOfValue(value: JObject): Integer; cdecl;
    function keyAt(index: Integer): Int64; cdecl;
    procedure put(key: Int64; value: JObject); cdecl;
    procedure remove(key: Int64); cdecl;
    procedure removeAt(index: Integer); cdecl;
    procedure setValueAt(index: Integer; value: JObject); cdecl;
    function size: Integer; cdecl;
    function toString: JString; cdecl;
    function valueAt(index: Integer): JObject; cdecl;
  end;
  TJLongSparseArray = class(TJavaGenericImport<JLongSparseArrayClass, JLongSparseArray>) end;

  JPairClass = interface(JObjectClass)
    ['{FC74853D-BE83-4F13-8068-CA5D73548F17}']
    {class} function init(first: JObject; second: JObject): JPair; cdecl;
    {class} function create(a: JObject; b: JObject): JPair; cdecl;
  end;

  [JavaSignature('android/util/Pair')]
  JPair = interface(JObject)
    ['{F5066B7F-AF96-45FD-AF9F-7F42F8EE8A5D}']
    function _Getfirst: JObject; cdecl;
    function _Getsecond: JObject; cdecl;
    function equals(o: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    property first: JObject read _Getfirst;
    property second: JObject read _Getsecond;
  end;
  TJPair = class(TJavaGenericImport<JPairClass, JPair>) end;

  JPrinterClass = interface(IJavaClass)
    ['{7D16885A-30FC-4B75-A5B5-78A36D20B8CC}']
  end;

  [JavaSignature('android/util/Printer')]
  JPrinter = interface(IJavaInstance)
    ['{118D609E-31BE-43D6-91A8-119D5B6BBE3C}']
    procedure println(x: JString); cdecl;
  end;
  TJPrinter = class(TJavaGenericImport<JPrinterClass, JPrinter>) end;

  JPropertyClass = interface(JObjectClass)
    ['{9189FC50-84D8-4091-95ED-15B3B8FACC15}']
    {class} function init(type_: Jlang_Class; name: JString): JProperty; cdecl;
    {class} function &of(hostType: Jlang_Class; valueType: Jlang_Class; name: JString): JProperty; cdecl;
  end;

  [JavaSignature('android/util/Property')]
  JProperty = interface(JObject)
    ['{2803C5A6-4347-4D46-AEAA-28C87BB457B5}']
    function &get(object_: JObject): JObject; cdecl;
    function getName: JString; cdecl;
    function getType: Jlang_Class; cdecl;
    function isReadOnly: Boolean; cdecl;
    procedure &set(object_: JObject; value: JObject); cdecl;
  end;
  TJProperty = class(TJavaGenericImport<JPropertyClass, JProperty>) end;

  JRangeClass = interface(JObjectClass)
    ['{F33DC57E-796A-44C1-81F6-0DE129721343}']
    {class} function init(lower: JComparable; upper: JComparable): JRange; cdecl;
    {class} function create(lower: JComparable; upper: JComparable): JRange; cdecl;
  end;

  [JavaSignature('android/util/Range')]
  JRange = interface(JObject)
    ['{CCDFBCEC-E08B-4B45-9953-F04EDBCE1AC7}']
    function clamp(value: JComparable): JComparable; cdecl;
    function &contains(value: JComparable): Boolean; cdecl; overload;
    function &contains(range: JRange): Boolean; cdecl; overload;
    function equals(obj: JObject): Boolean; cdecl;
    function extend(range: JRange): JRange; cdecl; overload;
    function extend(lower: JComparable; upper: JComparable): JRange; cdecl; overload;
    function extend(value: JComparable): JRange; cdecl; overload;
    function getLower: JComparable; cdecl;
    function getUpper: JComparable; cdecl;
    function hashCode: Integer; cdecl;
    function intersect(range: JRange): JRange; cdecl; overload;
    function intersect(lower: JComparable; upper: JComparable): JRange; cdecl; overload;
    function toString: JString; cdecl;
  end;
  TJRange = class(TJavaGenericImport<JRangeClass, JRange>) end;

  JRationalClass = interface(JNumberClass)
    ['{592776A4-0BC8-4829-88B1-714E90C34FE1}']
    {class} function _GetNEGATIVE_INFINITY: JRational; cdecl;
    {class} function _GetNaN: JRational; cdecl;
    {class} function _GetPOSITIVE_INFINITY: JRational; cdecl;
    {class} function _GetZERO: JRational; cdecl;
    {class} function init(numerator: Integer; denominator: Integer): JRational; cdecl;
    {class} function parseRational(string_: JString): JRational; cdecl;
    {class} property NEGATIVE_INFINITY: JRational read _GetNEGATIVE_INFINITY;
    {class} property NaN: JRational read _GetNaN;
    {class} property POSITIVE_INFINITY: JRational read _GetPOSITIVE_INFINITY;
    {class} property ZERO: JRational read _GetZERO;
  end;

  [JavaSignature('android/util/Rational')]
  JRational = interface(JNumber)
    ['{5DEFEB1B-2D1F-4ADF-B69B-54B0498F8544}']
    function compareTo(another: JRational): Integer; cdecl;
    function doubleValue: Double; cdecl;
    function equals(obj: JObject): Boolean; cdecl;
    function floatValue: Single; cdecl;
    function getDenominator: Integer; cdecl;
    function getNumerator: Integer; cdecl;
    function hashCode: Integer; cdecl;
    function intValue: Integer; cdecl;
    function isFinite: Boolean; cdecl;
    function isInfinite: Boolean; cdecl;
    function isNaN: Boolean; cdecl;
    function isZero: Boolean; cdecl;
    function longValue: Int64; cdecl;
    function shortValue: SmallInt; cdecl;
    function toString: JString; cdecl;
  end;
  TJRational = class(TJavaGenericImport<JRationalClass, JRational>) end;

  Jutil_SizeClass = interface(JObjectClass)
    ['{214877DC-3C23-41FC-A5E5-AD122E30E787}']
    {class} function init(width: Integer; height: Integer): Jutil_Size; cdecl;
    {class} function parseSize(string_: JString): Jutil_Size; cdecl;
  end;

  [JavaSignature('android/util/Size')]
  Jutil_Size = interface(JObject)
    ['{E57F5407-74DB-4B6C-A0D0-3AE4B7A33C28}']
    function equals(obj: JObject): Boolean; cdecl;
    function getHeight: Integer; cdecl;
    function getWidth: Integer; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJutil_Size = class(TJavaGenericImport<Jutil_SizeClass, Jutil_Size>) end;

  JSizeFClass = interface(JObjectClass)
    ['{25EA8EC9-E533-4443-AE91-AD7A5F818865}']
    //{class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init(width: Single; height: Single): JSizeF; cdecl;
    {class} function parseSizeF(string_: JString): JSizeF; cdecl;
    //{class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/util/SizeF')]
  JSizeF = interface(JObject)
    ['{4EEB0336-42B1-4281-880C-D43693201935}']
    function describeContents: Integer; cdecl;
    function equals(obj: JObject): Boolean; cdecl;
    function getHeight: Single; cdecl;
    function getWidth: Single; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    //procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  end;
  TJSizeF = class(TJavaGenericImport<JSizeFClass, JSizeF>) end;

  JSparseArrayClass = interface(JObjectClass)
    ['{4E51484A-CD9E-4170-8CCF-B3C8A8CA3F26}']
    {class} function init: JSparseArray; cdecl; overload;
    {class} function init(initialCapacity: Integer): JSparseArray; cdecl; overload;
  end;

  [JavaSignature('android/util/SparseArray')]
  JSparseArray = interface(JObject)
    ['{AFB1BC39-0C23-4B91-A7E9-C03FD965EA44}']
    procedure append(key: Integer; value: JObject); cdecl;
    procedure clear; cdecl;
    function clone: JSparseArray; cdecl;
    function &contains(key: Integer): Boolean; cdecl;
    function contentEquals(other: JSparseArray): Boolean; cdecl;
    function contentHashCode: Integer; cdecl;
    procedure delete(key: Integer); cdecl;
    function &get(key: Integer): JObject; cdecl; overload;
    function &get(key: Integer; valueIfKeyNotFound: JObject): JObject; cdecl; overload;
    function indexOfKey(key: Integer): Integer; cdecl;
    function indexOfValue(value: JObject): Integer; cdecl;
    function keyAt(index: Integer): Integer; cdecl;
    procedure put(key: Integer; value: JObject); cdecl;
    procedure remove(key: Integer); cdecl;
    procedure removeAt(index: Integer); cdecl;
    procedure removeAtRange(index: Integer; size: Integer); cdecl;
    procedure &set(key: Integer; value: JObject); cdecl;
    procedure setValueAt(index: Integer; value: JObject); cdecl;
    function size: Integer; cdecl;
    function toString: JString; cdecl;
    function valueAt(index: Integer): JObject; cdecl;
  end;
  TJSparseArray = class(TJavaGenericImport<JSparseArrayClass, JSparseArray>) end;

  JSparseBooleanArrayClass = interface(JObjectClass)
    ['{D23BD7F9-76E0-4E9D-AAF4-AFE4660DEE0F}']
    {class} function init: JSparseBooleanArray; cdecl; overload;
    {class} function init(initialCapacity: Integer): JSparseBooleanArray; cdecl; overload;
  end;

  [JavaSignature('android/util/SparseBooleanArray')]
  JSparseBooleanArray = interface(JObject)
    ['{5DD9C005-5426-4BEB-A03E-CA66DE454B01}']
    procedure append(key: Integer; value: Boolean); cdecl;
    procedure clear; cdecl;
    function clone: JSparseBooleanArray; cdecl;
    procedure delete(key: Integer); cdecl;
    function equals(that: JObject): Boolean; cdecl;
    function &get(key: Integer): Boolean; cdecl; overload;
    function &get(key: Integer; valueIfKeyNotFound: Boolean): Boolean; cdecl; overload;
    function hashCode: Integer; cdecl;
    function indexOfKey(key: Integer): Integer; cdecl;
    function indexOfValue(value: Boolean): Integer; cdecl;
    function keyAt(index: Integer): Integer; cdecl;
    procedure put(key: Integer; value: Boolean); cdecl;
    procedure removeAt(index: Integer); cdecl;
    procedure setValueAt(index: Integer; value: Boolean); cdecl;
    function size: Integer; cdecl;
    function toString: JString; cdecl;
    function valueAt(index: Integer): Boolean; cdecl;
  end;
  TJSparseBooleanArray = class(TJavaGenericImport<JSparseBooleanArrayClass, JSparseBooleanArray>) end;

  JTypedValueClass = interface(JObjectClass)
    ['{772C1CBF-A712-432D-9AAE-54E62D0DC745}']
    {class} function _GetCOMPLEX_MANTISSA_MASK: Integer; cdecl;
    {class} function _GetCOMPLEX_MANTISSA_SHIFT: Integer; cdecl;
    {class} function _GetCOMPLEX_RADIX_0p23: Integer; cdecl;
    {class} function _GetCOMPLEX_RADIX_16p7: Integer; cdecl;
    {class} function _GetCOMPLEX_RADIX_23p0: Integer; cdecl;
    {class} function _GetCOMPLEX_RADIX_8p15: Integer; cdecl;
    {class} function _GetCOMPLEX_RADIX_MASK: Integer; cdecl;
    {class} function _GetCOMPLEX_RADIX_SHIFT: Integer; cdecl;
    {class} function _GetCOMPLEX_UNIT_DIP: Integer; cdecl;
    {class} function _GetCOMPLEX_UNIT_FRACTION: Integer; cdecl;
    {class} function _GetCOMPLEX_UNIT_FRACTION_PARENT: Integer; cdecl;
    {class} function _GetCOMPLEX_UNIT_IN: Integer; cdecl;
    {class} function _GetCOMPLEX_UNIT_MASK: Integer; cdecl;
    {class} function _GetCOMPLEX_UNIT_MM: Integer; cdecl;
    {class} function _GetCOMPLEX_UNIT_PT: Integer; cdecl;
    {class} function _GetCOMPLEX_UNIT_PX: Integer; cdecl;
    {class} function _GetCOMPLEX_UNIT_SHIFT: Integer; cdecl;
    {class} function _GetCOMPLEX_UNIT_SP: Integer; cdecl;
    {class} function _GetDATA_NULL_EMPTY: Integer; cdecl;
    {class} function _GetDATA_NULL_UNDEFINED: Integer; cdecl;
    {class} function _GetDENSITY_DEFAULT: Integer; cdecl;
    {class} function _GetDENSITY_NONE: Integer; cdecl;
    {class} function _GetTYPE_ATTRIBUTE: Integer; cdecl;
    {class} function _GetTYPE_DIMENSION: Integer; cdecl;
    {class} function _GetTYPE_FIRST_COLOR_INT: Integer; cdecl;
    {class} function _GetTYPE_FIRST_INT: Integer; cdecl;
    {class} function _GetTYPE_FLOAT: Integer; cdecl;
    {class} function _GetTYPE_FRACTION: Integer; cdecl;
    {class} function _GetTYPE_INT_BOOLEAN: Integer; cdecl;
    {class} function _GetTYPE_INT_COLOR_ARGB4: Integer; cdecl;
    {class} function _GetTYPE_INT_COLOR_ARGB8: Integer; cdecl;
    {class} function _GetTYPE_INT_COLOR_RGB4: Integer; cdecl;
    {class} function _GetTYPE_INT_COLOR_RGB8: Integer; cdecl;
    {class} function _GetTYPE_INT_DEC: Integer; cdecl;
    {class} function _GetTYPE_INT_HEX: Integer; cdecl;
    {class} function _GetTYPE_LAST_COLOR_INT: Integer; cdecl;
    {class} function _GetTYPE_LAST_INT: Integer; cdecl;
    {class} function _GetTYPE_NULL: Integer; cdecl;
    {class} function _GetTYPE_REFERENCE: Integer; cdecl;
    {class} function _GetTYPE_STRING: Integer; cdecl;
    {class} function init: JTypedValue; cdecl;
    {class} function applyDimension(unit_: Integer; value: Single; metrics: JDisplayMetrics): Single; cdecl;
    {class} function coerceToString(type_: Integer; data: Integer): JString; cdecl; overload;
    {class} function complexToDimension(data: Integer; metrics: JDisplayMetrics): Single; cdecl;
    {class} function complexToDimensionPixelOffset(data: Integer; metrics: JDisplayMetrics): Integer; cdecl;
    {class} function complexToDimensionPixelSize(data: Integer; metrics: JDisplayMetrics): Integer; cdecl;
    {class} function complexToFloat(complex: Integer): Single; cdecl;
    {class} function complexToFraction(data: Integer; base: Single; pbase: Single): Single; cdecl;
    {class} property COMPLEX_MANTISSA_MASK: Integer read _GetCOMPLEX_MANTISSA_MASK;
    {class} property COMPLEX_MANTISSA_SHIFT: Integer read _GetCOMPLEX_MANTISSA_SHIFT;
    {class} property COMPLEX_RADIX_0p23: Integer read _GetCOMPLEX_RADIX_0p23;
    {class} property COMPLEX_RADIX_16p7: Integer read _GetCOMPLEX_RADIX_16p7;
    {class} property COMPLEX_RADIX_23p0: Integer read _GetCOMPLEX_RADIX_23p0;
    {class} property COMPLEX_RADIX_8p15: Integer read _GetCOMPLEX_RADIX_8p15;
    {class} property COMPLEX_RADIX_MASK: Integer read _GetCOMPLEX_RADIX_MASK;
    {class} property COMPLEX_RADIX_SHIFT: Integer read _GetCOMPLEX_RADIX_SHIFT;
    {class} property COMPLEX_UNIT_DIP: Integer read _GetCOMPLEX_UNIT_DIP;
    {class} property COMPLEX_UNIT_FRACTION: Integer read _GetCOMPLEX_UNIT_FRACTION;
    {class} property COMPLEX_UNIT_FRACTION_PARENT: Integer read _GetCOMPLEX_UNIT_FRACTION_PARENT;
    {class} property COMPLEX_UNIT_IN: Integer read _GetCOMPLEX_UNIT_IN;
    {class} property COMPLEX_UNIT_MASK: Integer read _GetCOMPLEX_UNIT_MASK;
    {class} property COMPLEX_UNIT_MM: Integer read _GetCOMPLEX_UNIT_MM;
    {class} property COMPLEX_UNIT_PT: Integer read _GetCOMPLEX_UNIT_PT;
    {class} property COMPLEX_UNIT_PX: Integer read _GetCOMPLEX_UNIT_PX;
    {class} property COMPLEX_UNIT_SHIFT: Integer read _GetCOMPLEX_UNIT_SHIFT;
    {class} property COMPLEX_UNIT_SP: Integer read _GetCOMPLEX_UNIT_SP;
    {class} property DATA_NULL_EMPTY: Integer read _GetDATA_NULL_EMPTY;
    {class} property DATA_NULL_UNDEFINED: Integer read _GetDATA_NULL_UNDEFINED;
    {class} property DENSITY_DEFAULT: Integer read _GetDENSITY_DEFAULT;
    {class} property DENSITY_NONE: Integer read _GetDENSITY_NONE;
    {class} property TYPE_ATTRIBUTE: Integer read _GetTYPE_ATTRIBUTE;
    {class} property TYPE_DIMENSION: Integer read _GetTYPE_DIMENSION;
    {class} property TYPE_FIRST_COLOR_INT: Integer read _GetTYPE_FIRST_COLOR_INT;
    {class} property TYPE_FIRST_INT: Integer read _GetTYPE_FIRST_INT;
    {class} property TYPE_FLOAT: Integer read _GetTYPE_FLOAT;
    {class} property TYPE_FRACTION: Integer read _GetTYPE_FRACTION;
    {class} property TYPE_INT_BOOLEAN: Integer read _GetTYPE_INT_BOOLEAN;
    {class} property TYPE_INT_COLOR_ARGB4: Integer read _GetTYPE_INT_COLOR_ARGB4;
    {class} property TYPE_INT_COLOR_ARGB8: Integer read _GetTYPE_INT_COLOR_ARGB8;
    {class} property TYPE_INT_COLOR_RGB4: Integer read _GetTYPE_INT_COLOR_RGB4;
    {class} property TYPE_INT_COLOR_RGB8: Integer read _GetTYPE_INT_COLOR_RGB8;
    {class} property TYPE_INT_DEC: Integer read _GetTYPE_INT_DEC;
    {class} property TYPE_INT_HEX: Integer read _GetTYPE_INT_HEX;
    {class} property TYPE_LAST_COLOR_INT: Integer read _GetTYPE_LAST_COLOR_INT;
    {class} property TYPE_LAST_INT: Integer read _GetTYPE_LAST_INT;
    {class} property TYPE_NULL: Integer read _GetTYPE_NULL;
    {class} property TYPE_REFERENCE: Integer read _GetTYPE_REFERENCE;
    {class} property TYPE_STRING: Integer read _GetTYPE_STRING;
  end;

  [JavaSignature('android/util/TypedValue')]
  JTypedValue = interface(JObject)
    ['{776A314F-EFF7-401A-A02D-7FF0AA43F435}']
    function _GetassetCookie: Integer; cdecl;
    procedure _SetassetCookie(Value: Integer); cdecl;
    function _GetchangingConfigurations: Integer; cdecl;
    procedure _SetchangingConfigurations(Value: Integer); cdecl;
    function _Getdata: Integer; cdecl;
    procedure _Setdata(Value: Integer); cdecl;
    function _Getdensity: Integer; cdecl;
    procedure _Setdensity(Value: Integer); cdecl;
    function _GetresourceId: Integer; cdecl;
    procedure _SetresourceId(Value: Integer); cdecl;
    function _GetsourceResourceId: Integer; cdecl;
    procedure _SetsourceResourceId(Value: Integer); cdecl;
    function _Getstring: JCharSequence; cdecl;
    procedure _Setstring(Value: JCharSequence); cdecl;
    function _Gettype: Integer; cdecl;
    procedure _Settype(Value: Integer); cdecl;
    function coerceToString: JCharSequence; cdecl; overload;
    function getComplexUnit: Integer; cdecl;
    function getDimension(metrics: JDisplayMetrics): Single; cdecl;
    function getFloat: Single; cdecl;
    function getFraction(base: Single; pbase: Single): Single; cdecl;
    function isColorType: Boolean; cdecl;
    procedure setTo(other: JTypedValue); cdecl;
    function toString: JString; cdecl;
    property assetCookie: Integer read _GetassetCookie write _SetassetCookie;
    property changingConfigurations: Integer read _GetchangingConfigurations write _SetchangingConfigurations;
    property data: Integer read _Getdata write _Setdata;
    property density: Integer read _Getdensity write _Setdensity;
    property resourceId: Integer read _GetresourceId write _SetresourceId;
    property sourceResourceId: Integer read _GetsourceResourceId write _SetsourceResourceId;
    property &string: JCharSequence read _Getstring write _Setstring;
    property &type: Integer read _Gettype write _Settype;
  end;
  TJTypedValue = class(TJavaGenericImport<JTypedValueClass, JTypedValue>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Androidapi.JNI.Util.JAndroidException', TypeInfo(Androidapi.JNI.Util.JAndroidException));
  TRegTypes.RegisterType('Androidapi.JNI.Util.JAndroidRuntimeException', TypeInfo(Androidapi.JNI.Util.JAndroidRuntimeException));
  TRegTypes.RegisterType('Androidapi.JNI.Util.Jutil_ArrayMap', TypeInfo(Androidapi.JNI.Util.Jutil_ArrayMap));
  TRegTypes.RegisterType('Androidapi.JNI.Util.JAttributeSet', TypeInfo(Androidapi.JNI.Util.JAttributeSet));
  TRegTypes.RegisterType('Androidapi.JNI.Util.JDisplayMetrics', TypeInfo(Androidapi.JNI.Util.JDisplayMetrics));
  TRegTypes.RegisterType('Androidapi.JNI.Util.Jutil_Log', TypeInfo(Androidapi.JNI.Util.Jutil_Log));
  TRegTypes.RegisterType('Androidapi.JNI.Util.JLongSparseArray', TypeInfo(Androidapi.JNI.Util.JLongSparseArray));
  TRegTypes.RegisterType('Androidapi.JNI.Util.JPair', TypeInfo(Androidapi.JNI.Util.JPair));
  TRegTypes.RegisterType('Androidapi.JNI.Util.JPrinter', TypeInfo(Androidapi.JNI.Util.JPrinter));
  TRegTypes.RegisterType('Androidapi.JNI.Util.JProperty', TypeInfo(Androidapi.JNI.Util.JProperty));
  TRegTypes.RegisterType('Androidapi.JNI.Util.JRange', TypeInfo(Androidapi.JNI.Util.JRange));
  TRegTypes.RegisterType('Androidapi.JNI.Util.JRational', TypeInfo(Androidapi.JNI.Util.JRational));
  TRegTypes.RegisterType('Androidapi.JNI.Util.Jutil_Size', TypeInfo(Androidapi.JNI.Util.Jutil_Size));
  TRegTypes.RegisterType('Androidapi.JNI.Util.JSizeF', TypeInfo(Androidapi.JNI.Util.JSizeF));
  TRegTypes.RegisterType('Androidapi.JNI.Util.JSparseArray', TypeInfo(Androidapi.JNI.Util.JSparseArray));
  TRegTypes.RegisterType('Androidapi.JNI.Util.JSparseBooleanArray', TypeInfo(Androidapi.JNI.Util.JSparseBooleanArray));
  TRegTypes.RegisterType('Androidapi.JNI.Util.JTypedValue', TypeInfo(Androidapi.JNI.Util.JTypedValue));
end;

initialization
  RegisterTypes;
end.


