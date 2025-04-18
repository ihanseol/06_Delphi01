{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Location;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.Util;

type
// ===== Forward declarations =====

  JAddress = interface;//android.location.Address
  JCriteria = interface;//android.location.Criteria
  JGeocoder = interface;//android.location.Geocoder
  JGeocoder_GeocodeListener = interface;//android.location.Geocoder$GeocodeListener
  JGnssAntennaInfo = interface;//android.location.GnssAntennaInfo
  JGnssAntennaInfo_Builder = interface;//android.location.GnssAntennaInfo$Builder
  JGnssAntennaInfo_Listener = interface;//android.location.GnssAntennaInfo$Listener
  JGnssAntennaInfo_PhaseCenterOffset = interface;//android.location.GnssAntennaInfo$PhaseCenterOffset
  JGnssAntennaInfo_SphericalCorrections = interface;//android.location.GnssAntennaInfo$SphericalCorrections
  JGnssAutomaticGainControl = interface;//android.location.GnssAutomaticGainControl
  JGnssAutomaticGainControl_Builder = interface;//android.location.GnssAutomaticGainControl$Builder
  JGnssCapabilities = interface;//android.location.GnssCapabilities
  JGnssCapabilities_Builder = interface;//android.location.GnssCapabilities$Builder
  JGnssClock = interface;//android.location.GnssClock
  JGnssMeasurement = interface;//android.location.GnssMeasurement
  JGnssMeasurementRequest = interface;//android.location.GnssMeasurementRequest
  JGnssMeasurementRequest_Builder = interface;//android.location.GnssMeasurementRequest$Builder
  JGnssMeasurementsEvent = interface;//android.location.GnssMeasurementsEvent
  JGnssMeasurementsEvent_Builder = interface;//android.location.GnssMeasurementsEvent$Builder
  JGnssMeasurementsEvent_Callback = interface;//android.location.GnssMeasurementsEvent$Callback
  JGnssNavigationMessage = interface;//android.location.GnssNavigationMessage
  JGnssNavigationMessage_Callback = interface;//android.location.GnssNavigationMessage$Callback
  JGnssStatus = interface;//android.location.GnssStatus
  JGnssStatus_Builder = interface;//android.location.GnssStatus$Builder
  JGnssStatus_Callback = interface;//android.location.GnssStatus$Callback
  JGpsSatellite = interface;//android.location.GpsSatellite
  JGpsStatus = interface;//android.location.GpsStatus
  JGpsStatus_Listener = interface;//android.location.GpsStatus$Listener
  JGpsStatus_NmeaListener = interface;//android.location.GpsStatus$NmeaListener
  JLocation = interface;//android.location.Location
  JLocationListener = interface;//android.location.LocationListener
  JLocationManager = interface;//android.location.LocationManager
  JLocationProvider = interface;//android.location.LocationProvider
  Jlocation_LocationRequest = interface;//android.location.LocationRequest
  JLocationRequest_Builder = interface;//android.location.LocationRequest$Builder
  JOnNmeaMessageListener = interface;//android.location.OnNmeaMessageListener
  JSettingInjectorService = interface;//android.location.SettingInjectorService
  JProviderProperties = interface;//android.location.provider.ProviderProperties
  JProviderProperties_Builder = interface;//android.location.provider.ProviderProperties$Builder

// ===== Interface declarations =====

  JAddressClass = interface(JObjectClass)
    ['{11E2A62D-94CD-4C84-9BAB-651BDB8AD89F}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init(locale: JLocale): JAddress; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/location/Address')]
  JAddress = interface(JObject)
    ['{BEDE6B0A-3FCA-4A73-BC74-0FB86CE8E02F}']
    procedure clearLatitude; cdecl;
    procedure clearLongitude; cdecl;
    function describeContents: Integer; cdecl;
    function getAddressLine(index: Integer): JString; cdecl;
    function getAdminArea: JString; cdecl;
    function getCountryCode: JString; cdecl;
    function getCountryName: JString; cdecl;
    function getExtras: JBundle; cdecl;
    function getFeatureName: JString; cdecl;
    function getLatitude: Double; cdecl;
    function getLocale: JLocale; cdecl;
    function getLocality: JString; cdecl;
    function getLongitude: Double; cdecl;
    function getMaxAddressLineIndex: Integer; cdecl;
    function getPhone: JString; cdecl;
    function getPostalCode: JString; cdecl;
    function getPremises: JString; cdecl;
    function getSubAdminArea: JString; cdecl;
    function getSubLocality: JString; cdecl;
    function getSubThoroughfare: JString; cdecl;
    function getThoroughfare: JString; cdecl;
    function getUrl: JString; cdecl;
    function hasLatitude: Boolean; cdecl;
    function hasLongitude: Boolean; cdecl;
    procedure setAddressLine(index: Integer; line: JString); cdecl;
    procedure setAdminArea(adminArea: JString); cdecl;
    procedure setCountryCode(countryCode: JString); cdecl;
    procedure setCountryName(countryName: JString); cdecl;
    procedure setExtras(extras: JBundle); cdecl;
    procedure setFeatureName(featureName: JString); cdecl;
    procedure setLatitude(latitude: Double); cdecl;
    procedure setLocality(locality: JString); cdecl;
    procedure setLongitude(longitude: Double); cdecl;
    procedure setPhone(phone: JString); cdecl;
    procedure setPostalCode(postalCode: JString); cdecl;
    procedure setPremises(premises: JString); cdecl;
    procedure setSubAdminArea(subAdminArea: JString); cdecl;
    procedure setSubLocality(sublocality: JString); cdecl;
    procedure setSubThoroughfare(subthoroughfare: JString); cdecl;
    procedure setThoroughfare(thoroughfare: JString); cdecl;
    procedure setUrl(Url: JString); cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJAddress = class(TJavaGenericImport<JAddressClass, JAddress>) end;

  JCriteriaClass = interface(JObjectClass)
    ['{5A00B9BC-1915-4D8F-A077-CED7234A64D1}']
    {class} function _GetACCURACY_COARSE: Integer; cdecl;
    {class} function _GetACCURACY_FINE: Integer; cdecl;
    {class} function _GetACCURACY_HIGH: Integer; cdecl;
    {class} function _GetACCURACY_LOW: Integer; cdecl;
    {class} function _GetACCURACY_MEDIUM: Integer; cdecl;
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetNO_REQUIREMENT: Integer; cdecl;
    {class} function _GetPOWER_HIGH: Integer; cdecl;
    {class} function _GetPOWER_LOW: Integer; cdecl;
    {class} function _GetPOWER_MEDIUM: Integer; cdecl;
    {class} function init: JCriteria; cdecl; overload;
    {class} function init(criteria: JCriteria): JCriteria; cdecl; overload;
    {class} property ACCURACY_COARSE: Integer read _GetACCURACY_COARSE;
    {class} property ACCURACY_FINE: Integer read _GetACCURACY_FINE;
    {class} property ACCURACY_HIGH: Integer read _GetACCURACY_HIGH;
    {class} property ACCURACY_LOW: Integer read _GetACCURACY_LOW;
    {class} property ACCURACY_MEDIUM: Integer read _GetACCURACY_MEDIUM;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property NO_REQUIREMENT: Integer read _GetNO_REQUIREMENT;
    {class} property POWER_HIGH: Integer read _GetPOWER_HIGH;
    {class} property POWER_LOW: Integer read _GetPOWER_LOW;
    {class} property POWER_MEDIUM: Integer read _GetPOWER_MEDIUM;
  end;

  [JavaSignature('android/location/Criteria')]
  JCriteria = interface(JObject)
    ['{105327BB-E655-4565-BCB8-796CB73B811C}']
    function describeContents: Integer; cdecl;
    function getAccuracy: Integer; cdecl;
    function getBearingAccuracy: Integer; cdecl;
    function getHorizontalAccuracy: Integer; cdecl;
    function getPowerRequirement: Integer; cdecl;
    function getSpeedAccuracy: Integer; cdecl;
    function getVerticalAccuracy: Integer; cdecl;
    function isAltitudeRequired: Boolean; cdecl;
    function isBearingRequired: Boolean; cdecl;
    function isCostAllowed: Boolean; cdecl;
    function isSpeedRequired: Boolean; cdecl;
    procedure setAccuracy(accuracy: Integer); cdecl;
    procedure setAltitudeRequired(altitudeRequired: Boolean); cdecl;
    procedure setBearingAccuracy(accuracy: Integer); cdecl;
    procedure setBearingRequired(bearingRequired: Boolean); cdecl;
    procedure setCostAllowed(costAllowed: Boolean); cdecl;
    procedure setHorizontalAccuracy(accuracy: Integer); cdecl;
    procedure setPowerRequirement(powerRequirement: Integer); cdecl;
    procedure setSpeedAccuracy(accuracy: Integer); cdecl;
    procedure setSpeedRequired(speedRequired: Boolean); cdecl;
    procedure setVerticalAccuracy(accuracy: Integer); cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJCriteria = class(TJavaGenericImport<JCriteriaClass, JCriteria>) end;

  JGeocoderClass = interface(JObjectClass)
    ['{A21F3CB7-F053-464B-B59B-EDD0E4C784F2}']
    {class} function init(context: JContext): JGeocoder; cdecl; overload;
    {class} function init(context: JContext; locale: JLocale): JGeocoder; cdecl; overload;
    {class} function isPresent: Boolean; cdecl;
  end;

  [JavaSignature('android/location/Geocoder')]
  JGeocoder = interface(JObject)
    ['{32343023-DA3D-41DE-9A9A-69935156D5B7}']
    function getFromLocation(latitude: Double; longitude: Double; maxResults: Integer): JList; cdecl; overload;//Deprecated
    procedure getFromLocation(latitude: Double; longitude: Double; maxResults: Integer; listener: JGeocoder_GeocodeListener); cdecl; overload;
    function getFromLocationName(locationName: JString; maxResults: Integer): JList; cdecl; overload;//Deprecated
    procedure getFromLocationName(locationName: JString; maxResults: Integer; listener: JGeocoder_GeocodeListener); cdecl; overload;
    function getFromLocationName(locationName: JString; maxResults: Integer; lowerLeftLatitude: Double; lowerLeftLongitude: Double; upperRightLatitude: Double; upperRightLongitude: Double): JList; cdecl; overload;//Deprecated
    procedure getFromLocationName(locationName: JString; maxResults: Integer; lowerLeftLatitude: Double; lowerLeftLongitude: Double; upperRightLatitude: Double; upperRightLongitude: Double; listener: JGeocoder_GeocodeListener); cdecl; overload;
  end;
  TJGeocoder = class(TJavaGenericImport<JGeocoderClass, JGeocoder>) end;

  JGeocoder_GeocodeListenerClass = interface(IJavaClass)
    ['{97E50C35-F895-4EB8-A906-C6EE13B85991}']
  end;

  [JavaSignature('android/location/Geocoder$GeocodeListener')]
  JGeocoder_GeocodeListener = interface(IJavaInstance)
    ['{2848EDF3-56CB-460B-90CF-69DEB9661552}']
    procedure onError(errorMessage: JString); cdecl;
    procedure onGeocode(addresses: JList); cdecl;
  end;
  TJGeocoder_GeocodeListener = class(TJavaGenericImport<JGeocoder_GeocodeListenerClass, JGeocoder_GeocodeListener>) end;

  JGnssAntennaInfoClass = interface(JObjectClass)
    ['{5FA974AD-FEDE-4420-8B2C-951213520A44}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/location/GnssAntennaInfo')]
  JGnssAntennaInfo = interface(JObject)
    ['{03F07F2C-1FF8-4277-8C3F-E6DB38861A60}']
    function describeContents: Integer; cdecl;
    function equals(o: JObject): Boolean; cdecl;
    function getCarrierFrequencyMHz: Double; cdecl;
    function getPhaseCenterOffset: JGnssAntennaInfo_PhaseCenterOffset; cdecl;
    function getPhaseCenterVariationCorrections: JGnssAntennaInfo_SphericalCorrections; cdecl;
    function getSignalGainCorrections: JGnssAntennaInfo_SphericalCorrections; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJGnssAntennaInfo = class(TJavaGenericImport<JGnssAntennaInfoClass, JGnssAntennaInfo>) end;

  JGnssAntennaInfo_BuilderClass = interface(JObjectClass)
    ['{89C51F19-575D-4F40-BB50-90A9472E93A2}']
    {class} function init: JGnssAntennaInfo_Builder; cdecl; overload;//Deprecated
    {class} function init(carrierFrequencyMHz: Double; phaseCenterOffset: JGnssAntennaInfo_PhaseCenterOffset): JGnssAntennaInfo_Builder; cdecl; overload;
    {class} function init(antennaInfo: JGnssAntennaInfo): JGnssAntennaInfo_Builder; cdecl; overload;
  end;

  [JavaSignature('android/location/GnssAntennaInfo$Builder')]
  JGnssAntennaInfo_Builder = interface(JObject)
    ['{495E7863-86B4-4ADD-92CD-561C9F655A3B}']
    function build: JGnssAntennaInfo; cdecl;
    function setCarrierFrequencyMHz(carrierFrequencyMHz: Double): JGnssAntennaInfo_Builder; cdecl;
    function setPhaseCenterOffset(phaseCenterOffset: JGnssAntennaInfo_PhaseCenterOffset): JGnssAntennaInfo_Builder; cdecl;
    function setPhaseCenterVariationCorrections(phaseCenterVariationCorrections: JGnssAntennaInfo_SphericalCorrections): JGnssAntennaInfo_Builder; cdecl;
    function setSignalGainCorrections(signalGainCorrections: JGnssAntennaInfo_SphericalCorrections): JGnssAntennaInfo_Builder; cdecl;
  end;
  TJGnssAntennaInfo_Builder = class(TJavaGenericImport<JGnssAntennaInfo_BuilderClass, JGnssAntennaInfo_Builder>) end;

  JGnssAntennaInfo_ListenerClass = interface(IJavaClass)
    ['{674D5AF5-4CE0-4878-BC89-F410148E791C}']
  end;

  [JavaSignature('android/location/GnssAntennaInfo$Listener')]
  JGnssAntennaInfo_Listener = interface(IJavaInstance)
    ['{D6C70F8F-49D2-4F59-ABB6-8E9BE1621FFB}']
    procedure onGnssAntennaInfoReceived(gnssAntennaInfos: JList); cdecl;
  end;
  TJGnssAntennaInfo_Listener = class(TJavaGenericImport<JGnssAntennaInfo_ListenerClass, JGnssAntennaInfo_Listener>) end;

  JGnssAntennaInfo_PhaseCenterOffsetClass = interface(JObjectClass)
    ['{944068C1-019B-4D26-90A4-BD770688625D}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init(offsetXMm: Double; offsetXUncertaintyMm: Double; offsetYMm: Double; offsetYUncertaintyMm: Double; offsetZMm: Double; offsetZUncertaintyMm: Double): JGnssAntennaInfo_PhaseCenterOffset; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/location/GnssAntennaInfo$PhaseCenterOffset')]
  JGnssAntennaInfo_PhaseCenterOffset = interface(JObject)
    ['{D383FD18-F8B8-4C24-961D-A6E4CF537B33}']
    function describeContents: Integer; cdecl;
    function equals(o: JObject): Boolean; cdecl;
    function getXOffsetMm: Double; cdecl;
    function getXOffsetUncertaintyMm: Double; cdecl;
    function getYOffsetMm: Double; cdecl;
    function getYOffsetUncertaintyMm: Double; cdecl;
    function getZOffsetMm: Double; cdecl;
    function getZOffsetUncertaintyMm: Double; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJGnssAntennaInfo_PhaseCenterOffset = class(TJavaGenericImport<JGnssAntennaInfo_PhaseCenterOffsetClass, JGnssAntennaInfo_PhaseCenterOffset>) end;

  JGnssAntennaInfo_SphericalCorrectionsClass = interface(JObjectClass)
    ['{7ECEB2D0-AC1A-44DE-AC15-CECCF1E33280}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init(corrections: TJavaBiArray<Double>; correctionUncertainties: TJavaBiArray<Double>): JGnssAntennaInfo_SphericalCorrections; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/location/GnssAntennaInfo$SphericalCorrections')]
  JGnssAntennaInfo_SphericalCorrections = interface(JObject)
    ['{ECD22EF3-CCAA-4B28-9B2A-AAF9FF9BE532}']
    function describeContents: Integer; cdecl;
    function equals(o: JObject): Boolean; cdecl;
    function getCorrectionUncertaintiesArray: TJavaBiArray<Double>; cdecl;
    function getCorrectionsArray: TJavaBiArray<Double>; cdecl;
    function getDeltaPhi: Double; cdecl;
    function getDeltaTheta: Double; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJGnssAntennaInfo_SphericalCorrections = class(TJavaGenericImport<JGnssAntennaInfo_SphericalCorrectionsClass, JGnssAntennaInfo_SphericalCorrections>) end;

  JGnssAutomaticGainControlClass = interface(JObjectClass)
    ['{2C175815-DFB1-49AC-8ADD-E9740EE730B8}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/location/GnssAutomaticGainControl')]
  JGnssAutomaticGainControl = interface(JObject)
    ['{165D162B-CAF3-46F1-822B-39C032D1539E}']
    function describeContents: Integer; cdecl;
    function equals(obj: JObject): Boolean; cdecl;
    function getCarrierFrequencyHz: Int64; cdecl;
    function getConstellationType: Integer; cdecl;
    function getLevelDb: Double; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flag: Integer); cdecl;
  end;
  TJGnssAutomaticGainControl = class(TJavaGenericImport<JGnssAutomaticGainControlClass, JGnssAutomaticGainControl>) end;

  JGnssAutomaticGainControl_BuilderClass = interface(JObjectClass)
    ['{C9174A87-1A38-48CD-A410-28729AE85CE9}']
    {class} function init: JGnssAutomaticGainControl_Builder; cdecl; overload;
    {class} function init(agc: JGnssAutomaticGainControl): JGnssAutomaticGainControl_Builder; cdecl; overload;
  end;

  [JavaSignature('android/location/GnssAutomaticGainControl$Builder')]
  JGnssAutomaticGainControl_Builder = interface(JObject)
    ['{1B75037E-DC3E-4951-8E46-6F80087202C7}']
    function build: JGnssAutomaticGainControl; cdecl;
    function setCarrierFrequencyHz(carrierFrequencyHz: Int64): JGnssAutomaticGainControl_Builder; cdecl;
    function setConstellationType(constellationType: Integer): JGnssAutomaticGainControl_Builder; cdecl;
    function setLevelDb(levelDb: Double): JGnssAutomaticGainControl_Builder; cdecl;
  end;
  TJGnssAutomaticGainControl_Builder = class(TJavaGenericImport<JGnssAutomaticGainControl_BuilderClass, JGnssAutomaticGainControl_Builder>) end;

  JGnssCapabilitiesClass = interface(JObjectClass)
    ['{A3CB1DDC-EF6C-4E68-9AE0-7F86F9497F02}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/location/GnssCapabilities')]
  JGnssCapabilities = interface(JObject)
    ['{49275F5F-E0B1-4B3E-B716-54165D2C01D7}']
    function describeContents: Integer; cdecl;
    function equals(o: JObject): Boolean; cdecl;
    function hasAntennaInfo: Boolean; cdecl;
    function hasGnssAntennaInfo: Boolean; cdecl;//Deprecated
    function hasMeasurements: Boolean; cdecl;
    function hasNavigationMessages: Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJGnssCapabilities = class(TJavaGenericImport<JGnssCapabilitiesClass, JGnssCapabilities>) end;

  JGnssCapabilities_BuilderClass = interface(JObjectClass)
    ['{4AEC872A-FF3F-4ADC-BCD3-E741A47E34D2}']
    {class} function init: JGnssCapabilities_Builder; cdecl; overload;
    {class} function init(capabilities: JGnssCapabilities): JGnssCapabilities_Builder; cdecl; overload;
  end;

  [JavaSignature('android/location/GnssCapabilities$Builder')]
  JGnssCapabilities_Builder = interface(JObject)
    ['{38D36825-302B-40DB-85FD-5A4FE6E9C17C}']
    function build: JGnssCapabilities; cdecl;
    function setHasAntennaInfo(capable: Boolean): JGnssCapabilities_Builder; cdecl;
    function setHasMeasurements(capable: Boolean): JGnssCapabilities_Builder; cdecl;
    function setHasNavigationMessages(capable: Boolean): JGnssCapabilities_Builder; cdecl;
  end;
  TJGnssCapabilities_Builder = class(TJavaGenericImport<JGnssCapabilities_BuilderClass, JGnssCapabilities_Builder>) end;

  JGnssClockClass = interface(JObjectClass)
    ['{09A9B58E-63C4-4283-B712-4FF9D704FA66}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/location/GnssClock')]
  JGnssClock = interface(JObject)
    ['{97181DBA-438C-4C89-816B-A8BC0B430941}']
    function describeContents: Integer; cdecl;
    function getBiasNanos: Double; cdecl;
    function getBiasUncertaintyNanos: Double; cdecl;
    function getDriftNanosPerSecond: Double; cdecl;
    function getDriftUncertaintyNanosPerSecond: Double; cdecl;
    function getElapsedRealtimeNanos: Int64; cdecl;
    function getElapsedRealtimeUncertaintyNanos: Double; cdecl;
    function getFullBiasNanos: Int64; cdecl;
    function getHardwareClockDiscontinuityCount: Integer; cdecl;
    function getLeapSecond: Integer; cdecl;
    function getReferenceCarrierFrequencyHzForIsb: Double; cdecl;
    function getReferenceCodeTypeForIsb: JString; cdecl;
    function getReferenceConstellationTypeForIsb: Integer; cdecl;
    function getTimeNanos: Int64; cdecl;
    function getTimeUncertaintyNanos: Double; cdecl;
    function hasBiasNanos: Boolean; cdecl;
    function hasBiasUncertaintyNanos: Boolean; cdecl;
    function hasDriftNanosPerSecond: Boolean; cdecl;
    function hasDriftUncertaintyNanosPerSecond: Boolean; cdecl;
    function hasElapsedRealtimeNanos: Boolean; cdecl;
    function hasElapsedRealtimeUncertaintyNanos: Boolean; cdecl;
    function hasFullBiasNanos: Boolean; cdecl;
    function hasLeapSecond: Boolean; cdecl;
    function hasReferenceCarrierFrequencyHzForIsb: Boolean; cdecl;
    function hasReferenceCodeTypeForIsb: Boolean; cdecl;
    function hasReferenceConstellationTypeForIsb: Boolean; cdecl;
    function hasTimeUncertaintyNanos: Boolean; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJGnssClock = class(TJavaGenericImport<JGnssClockClass, JGnssClock>) end;

  JGnssMeasurementClass = interface(JObjectClass)
    ['{9AFBF9A0-9DE6-4F98-8FBF-5DC142693ECA}']
    {class} function _GetADR_STATE_CYCLE_SLIP: Integer; cdecl;
    {class} function _GetADR_STATE_HALF_CYCLE_REPORTED: Integer; cdecl;
    {class} function _GetADR_STATE_HALF_CYCLE_RESOLVED: Integer; cdecl;
    {class} function _GetADR_STATE_RESET: Integer; cdecl;
    {class} function _GetADR_STATE_UNKNOWN: Integer; cdecl;
    {class} function _GetADR_STATE_VALID: Integer; cdecl;
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetMULTIPATH_INDICATOR_DETECTED: Integer; cdecl;
    {class} function _GetMULTIPATH_INDICATOR_NOT_DETECTED: Integer; cdecl;
    {class} function _GetMULTIPATH_INDICATOR_UNKNOWN: Integer; cdecl;
    {class} function _GetSTATE_2ND_CODE_LOCK: Integer; cdecl;
    {class} function _GetSTATE_BDS_D2_BIT_SYNC: Integer; cdecl;
    {class} function _GetSTATE_BDS_D2_SUBFRAME_SYNC: Integer; cdecl;
    {class} function _GetSTATE_BIT_SYNC: Integer; cdecl;
    {class} function _GetSTATE_CODE_LOCK: Integer; cdecl;
    {class} function _GetSTATE_GAL_E1BC_CODE_LOCK: Integer; cdecl;
    {class} function _GetSTATE_GAL_E1B_PAGE_SYNC: Integer; cdecl;
    {class} function _GetSTATE_GAL_E1C_2ND_CODE_LOCK: Integer; cdecl;
    {class} function _GetSTATE_GLO_STRING_SYNC: Integer; cdecl;
    {class} function _GetSTATE_GLO_TOD_DECODED: Integer; cdecl;
    {class} function _GetSTATE_GLO_TOD_KNOWN: Integer; cdecl;
    {class} function _GetSTATE_MSEC_AMBIGUOUS: Integer; cdecl;
    {class} function _GetSTATE_SBAS_SYNC: Integer; cdecl;
    {class} function _GetSTATE_SUBFRAME_SYNC: Integer; cdecl;
    {class} function _GetSTATE_SYMBOL_SYNC: Integer; cdecl;
    {class} function _GetSTATE_TOW_DECODED: Integer; cdecl;
    {class} function _GetSTATE_TOW_KNOWN: Integer; cdecl;
    {class} function _GetSTATE_UNKNOWN: Integer; cdecl;
    {class} property ADR_STATE_CYCLE_SLIP: Integer read _GetADR_STATE_CYCLE_SLIP;
    {class} property ADR_STATE_HALF_CYCLE_REPORTED: Integer read _GetADR_STATE_HALF_CYCLE_REPORTED;
    {class} property ADR_STATE_HALF_CYCLE_RESOLVED: Integer read _GetADR_STATE_HALF_CYCLE_RESOLVED;
    {class} property ADR_STATE_RESET: Integer read _GetADR_STATE_RESET;
    {class} property ADR_STATE_UNKNOWN: Integer read _GetADR_STATE_UNKNOWN;
    {class} property ADR_STATE_VALID: Integer read _GetADR_STATE_VALID;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property MULTIPATH_INDICATOR_DETECTED: Integer read _GetMULTIPATH_INDICATOR_DETECTED;
    {class} property MULTIPATH_INDICATOR_NOT_DETECTED: Integer read _GetMULTIPATH_INDICATOR_NOT_DETECTED;
    {class} property MULTIPATH_INDICATOR_UNKNOWN: Integer read _GetMULTIPATH_INDICATOR_UNKNOWN;
    {class} property STATE_2ND_CODE_LOCK: Integer read _GetSTATE_2ND_CODE_LOCK;
    {class} property STATE_BDS_D2_BIT_SYNC: Integer read _GetSTATE_BDS_D2_BIT_SYNC;
    {class} property STATE_BDS_D2_SUBFRAME_SYNC: Integer read _GetSTATE_BDS_D2_SUBFRAME_SYNC;
    {class} property STATE_BIT_SYNC: Integer read _GetSTATE_BIT_SYNC;
    {class} property STATE_CODE_LOCK: Integer read _GetSTATE_CODE_LOCK;
    {class} property STATE_GAL_E1BC_CODE_LOCK: Integer read _GetSTATE_GAL_E1BC_CODE_LOCK;
    {class} property STATE_GAL_E1B_PAGE_SYNC: Integer read _GetSTATE_GAL_E1B_PAGE_SYNC;
    {class} property STATE_GAL_E1C_2ND_CODE_LOCK: Integer read _GetSTATE_GAL_E1C_2ND_CODE_LOCK;
    {class} property STATE_GLO_STRING_SYNC: Integer read _GetSTATE_GLO_STRING_SYNC;
    {class} property STATE_GLO_TOD_DECODED: Integer read _GetSTATE_GLO_TOD_DECODED;
    {class} property STATE_GLO_TOD_KNOWN: Integer read _GetSTATE_GLO_TOD_KNOWN;
    {class} property STATE_MSEC_AMBIGUOUS: Integer read _GetSTATE_MSEC_AMBIGUOUS;
    {class} property STATE_SBAS_SYNC: Integer read _GetSTATE_SBAS_SYNC;
    {class} property STATE_SUBFRAME_SYNC: Integer read _GetSTATE_SUBFRAME_SYNC;
    {class} property STATE_SYMBOL_SYNC: Integer read _GetSTATE_SYMBOL_SYNC;
    {class} property STATE_TOW_DECODED: Integer read _GetSTATE_TOW_DECODED;
    {class} property STATE_TOW_KNOWN: Integer read _GetSTATE_TOW_KNOWN;
    {class} property STATE_UNKNOWN: Integer read _GetSTATE_UNKNOWN;
  end;

  [JavaSignature('android/location/GnssMeasurement')]
  JGnssMeasurement = interface(JObject)
    ['{B4FCE543-B01C-4CD6-836F-A4C4D3DC781A}']
    function describeContents: Integer; cdecl;
    function getAccumulatedDeltaRangeMeters: Double; cdecl;
    function getAccumulatedDeltaRangeState: Integer; cdecl;
    function getAccumulatedDeltaRangeUncertaintyMeters: Double; cdecl;
    function getAutomaticGainControlLevelDb: Double; cdecl;//Deprecated
    function getBasebandCn0DbHz: Double; cdecl;
    function getCarrierCycles: Int64; cdecl;//Deprecated
    function getCarrierFrequencyHz: Single; cdecl;
    function getCarrierPhase: Double; cdecl;//Deprecated
    function getCarrierPhaseUncertainty: Double; cdecl;//Deprecated
    function getCn0DbHz: Double; cdecl;
    function getCodeType: JString; cdecl;
    function getConstellationType: Integer; cdecl;
    function getFullInterSignalBiasNanos: Double; cdecl;
    function getFullInterSignalBiasUncertaintyNanos: Double; cdecl;
    function getMultipathIndicator: Integer; cdecl;
    function getPseudorangeRateMetersPerSecond: Double; cdecl;
    function getPseudorangeRateUncertaintyMetersPerSecond: Double; cdecl;
    function getReceivedSvTimeNanos: Int64; cdecl;
    function getReceivedSvTimeUncertaintyNanos: Int64; cdecl;
    function getSatelliteInterSignalBiasNanos: Double; cdecl;
    function getSatelliteInterSignalBiasUncertaintyNanos: Double; cdecl;
    function getSnrInDb: Double; cdecl;
    function getState: Integer; cdecl;
    function getSvid: Integer; cdecl;
    function getTimeOffsetNanos: Double; cdecl;
    function hasAutomaticGainControlLevelDb: Boolean; cdecl;//Deprecated
    function hasBasebandCn0DbHz: Boolean; cdecl;
    function hasCarrierCycles: Boolean; cdecl;//Deprecated
    function hasCarrierFrequencyHz: Boolean; cdecl;
    function hasCarrierPhase: Boolean; cdecl;//Deprecated
    function hasCarrierPhaseUncertainty: Boolean; cdecl;//Deprecated
    function hasCodeType: Boolean; cdecl;
    function hasFullInterSignalBiasNanos: Boolean; cdecl;
    function hasFullInterSignalBiasUncertaintyNanos: Boolean; cdecl;
    function hasSatelliteInterSignalBiasNanos: Boolean; cdecl;
    function hasSatelliteInterSignalBiasUncertaintyNanos: Boolean; cdecl;
    function hasSnrInDb: Boolean; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJGnssMeasurement = class(TJavaGenericImport<JGnssMeasurementClass, JGnssMeasurement>) end;

  JGnssMeasurementRequestClass = interface(JObjectClass)
    ['{FF6D8DFC-ADA1-4E47-BEF1-7A424349B994}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/location/GnssMeasurementRequest')]
  JGnssMeasurementRequest = interface(JObject)
    ['{476FF55B-261D-40FF-825D-5DFACAB00038}']
    function describeContents: Integer; cdecl;
    function equals(obj: JObject): Boolean; cdecl;
    function getIntervalMillis: Integer; cdecl;
    function hashCode: Integer; cdecl;
    function isFullTracking: Boolean; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJGnssMeasurementRequest = class(TJavaGenericImport<JGnssMeasurementRequestClass, JGnssMeasurementRequest>) end;

  JGnssMeasurementRequest_BuilderClass = interface(JObjectClass)
    ['{97EC585D-D33D-43ED-863E-591B1DBBED62}']
    {class} function init: JGnssMeasurementRequest_Builder; cdecl; overload;
    {class} function init(request: JGnssMeasurementRequest): JGnssMeasurementRequest_Builder; cdecl; overload;
  end;

  [JavaSignature('android/location/GnssMeasurementRequest$Builder')]
  JGnssMeasurementRequest_Builder = interface(JObject)
    ['{3537B1F4-54B1-40B9-9719-ACE4D2E1061F}']
    function build: JGnssMeasurementRequest; cdecl;
    function setFullTracking(value: Boolean): JGnssMeasurementRequest_Builder; cdecl;
    function setIntervalMillis(value: Integer): JGnssMeasurementRequest_Builder; cdecl;
  end;
  TJGnssMeasurementRequest_Builder = class(TJavaGenericImport<JGnssMeasurementRequest_BuilderClass, JGnssMeasurementRequest_Builder>) end;

  JGnssMeasurementsEventClass = interface(JObjectClass)
    ['{FFAD31A3-2C5C-4294-9F8E-68DBF2C45D0C}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/location/GnssMeasurementsEvent')]
  JGnssMeasurementsEvent = interface(JObject)
    ['{7ACE27AB-CD55-412D-B025-70B97FE3BED3}']
    function describeContents: Integer; cdecl;
    function getClock: JGnssClock; cdecl;
    function getGnssAutomaticGainControls: JCollection; cdecl;
    function getMeasurements: JCollection; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJGnssMeasurementsEvent = class(TJavaGenericImport<JGnssMeasurementsEventClass, JGnssMeasurementsEvent>) end;

  JGnssMeasurementsEvent_BuilderClass = interface(JObjectClass)
    ['{34481816-8883-46AC-AAF8-EC89B647BBBE}']
    {class} function init: JGnssMeasurementsEvent_Builder; cdecl; overload;
    {class} function init(event: JGnssMeasurementsEvent): JGnssMeasurementsEvent_Builder; cdecl; overload;
  end;

  [JavaSignature('android/location/GnssMeasurementsEvent$Builder')]
  JGnssMeasurementsEvent_Builder = interface(JObject)
    ['{51E50A86-E326-42B5-A21E-AA17E3C3ADF7}']
    function build: JGnssMeasurementsEvent; cdecl;
    function setClock(clock: JGnssClock): JGnssMeasurementsEvent_Builder; cdecl;
    function setGnssAutomaticGainControls(agcs: JCollection): JGnssMeasurementsEvent_Builder; cdecl;
    function setMeasurements(measurements: JCollection): JGnssMeasurementsEvent_Builder; cdecl;
  end;
  TJGnssMeasurementsEvent_Builder = class(TJavaGenericImport<JGnssMeasurementsEvent_BuilderClass, JGnssMeasurementsEvent_Builder>) end;

  JGnssMeasurementsEvent_CallbackClass = interface(JObjectClass)
    ['{1AC5BF62-7467-4D9D-BAF9-AE3D1D426497}']
    {class} function _GetSTATUS_LOCATION_DISABLED: Integer; cdecl;
    {class} function _GetSTATUS_NOT_ALLOWED: Integer; cdecl;
    {class} function _GetSTATUS_NOT_SUPPORTED: Integer; cdecl;
    {class} function _GetSTATUS_READY: Integer; cdecl;
    {class} function init: JGnssMeasurementsEvent_Callback; cdecl;
    {class} property STATUS_LOCATION_DISABLED: Integer read _GetSTATUS_LOCATION_DISABLED;
    {class} property STATUS_NOT_ALLOWED: Integer read _GetSTATUS_NOT_ALLOWED;
    {class} property STATUS_NOT_SUPPORTED: Integer read _GetSTATUS_NOT_SUPPORTED;
    {class} property STATUS_READY: Integer read _GetSTATUS_READY;
  end;

  [JavaSignature('android/location/GnssMeasurementsEvent$Callback')]
  JGnssMeasurementsEvent_Callback = interface(JObject)
    ['{391AB985-9C6D-4447-8C2A-B0064DA37B5C}']
    procedure onGnssMeasurementsReceived(eventArgs: JGnssMeasurementsEvent); cdecl;
    procedure onStatusChanged(status: Integer); cdecl;//Deprecated
  end;
  TJGnssMeasurementsEvent_Callback = class(TJavaGenericImport<JGnssMeasurementsEvent_CallbackClass, JGnssMeasurementsEvent_Callback>) end;

  JGnssNavigationMessageClass = interface(JObjectClass)
    ['{0522DC20-3E53-48E7-928F-30D296058D8A}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetSTATUS_PARITY_PASSED: Integer; cdecl;
    {class} function _GetSTATUS_PARITY_REBUILT: Integer; cdecl;
    {class} function _GetSTATUS_UNKNOWN: Integer; cdecl;
    {class} function _GetTYPE_BDS_CNAV1: Integer; cdecl;
    {class} function _GetTYPE_BDS_CNAV2: Integer; cdecl;
    {class} function _GetTYPE_BDS_D1: Integer; cdecl;
    {class} function _GetTYPE_BDS_D2: Integer; cdecl;
    {class} function _GetTYPE_GAL_F: Integer; cdecl;
    {class} function _GetTYPE_GAL_I: Integer; cdecl;
    {class} function _GetTYPE_GLO_L1CA: Integer; cdecl;
    {class} function _GetTYPE_GPS_CNAV2: Integer; cdecl;
    {class} function _GetTYPE_GPS_L1CA: Integer; cdecl;
    {class} function _GetTYPE_GPS_L2CNAV: Integer; cdecl;
    {class} function _GetTYPE_GPS_L5CNAV: Integer; cdecl;
    {class} function _GetTYPE_IRN_L5CA: Integer; cdecl;
    {class} function _GetTYPE_QZS_L1CA: Integer; cdecl;
    {class} function _GetTYPE_SBS: Integer; cdecl;
    {class} function _GetTYPE_UNKNOWN: Integer; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property STATUS_PARITY_PASSED: Integer read _GetSTATUS_PARITY_PASSED;
    {class} property STATUS_PARITY_REBUILT: Integer read _GetSTATUS_PARITY_REBUILT;
    {class} property STATUS_UNKNOWN: Integer read _GetSTATUS_UNKNOWN;
    {class} property TYPE_BDS_CNAV1: Integer read _GetTYPE_BDS_CNAV1;
    {class} property TYPE_BDS_CNAV2: Integer read _GetTYPE_BDS_CNAV2;
    {class} property TYPE_BDS_D1: Integer read _GetTYPE_BDS_D1;
    {class} property TYPE_BDS_D2: Integer read _GetTYPE_BDS_D2;
    {class} property TYPE_GAL_F: Integer read _GetTYPE_GAL_F;
    {class} property TYPE_GAL_I: Integer read _GetTYPE_GAL_I;
    {class} property TYPE_GLO_L1CA: Integer read _GetTYPE_GLO_L1CA;
    {class} property TYPE_GPS_CNAV2: Integer read _GetTYPE_GPS_CNAV2;
    {class} property TYPE_GPS_L1CA: Integer read _GetTYPE_GPS_L1CA;
    {class} property TYPE_GPS_L2CNAV: Integer read _GetTYPE_GPS_L2CNAV;
    {class} property TYPE_GPS_L5CNAV: Integer read _GetTYPE_GPS_L5CNAV;
    {class} property TYPE_IRN_L5CA: Integer read _GetTYPE_IRN_L5CA;
    {class} property TYPE_QZS_L1CA: Integer read _GetTYPE_QZS_L1CA;
    {class} property TYPE_SBS: Integer read _GetTYPE_SBS;
    {class} property TYPE_UNKNOWN: Integer read _GetTYPE_UNKNOWN;
  end;

  [JavaSignature('android/location/GnssNavigationMessage')]
  JGnssNavigationMessage = interface(JObject)
    ['{1E4F01B7-72EE-41B4-8C4C-090CBE87DDBD}']
    function describeContents: Integer; cdecl;
    function getData: TJavaArray<Byte>; cdecl;
    function getMessageId: Integer; cdecl;
    function getStatus: Integer; cdecl;
    function getSubmessageId: Integer; cdecl;
    function getSvid: Integer; cdecl;
    function getType: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJGnssNavigationMessage = class(TJavaGenericImport<JGnssNavigationMessageClass, JGnssNavigationMessage>) end;

  JGnssNavigationMessage_CallbackClass = interface(JObjectClass)
    ['{0180DD80-296E-4251-B279-DE2C905211AD}']
    {class} function _GetSTATUS_LOCATION_DISABLED: Integer; cdecl;
    {class} function _GetSTATUS_NOT_SUPPORTED: Integer; cdecl;
    {class} function _GetSTATUS_READY: Integer; cdecl;
    {class} function init: JGnssNavigationMessage_Callback; cdecl;
    {class} property STATUS_LOCATION_DISABLED: Integer read _GetSTATUS_LOCATION_DISABLED;
    {class} property STATUS_NOT_SUPPORTED: Integer read _GetSTATUS_NOT_SUPPORTED;
    {class} property STATUS_READY: Integer read _GetSTATUS_READY;
  end;

  [JavaSignature('android/location/GnssNavigationMessage$Callback')]
  JGnssNavigationMessage_Callback = interface(JObject)
    ['{D79C0144-2BDA-4C7C-A917-08BA125D056F}']
    procedure onGnssNavigationMessageReceived(event: JGnssNavigationMessage); cdecl;
    procedure onStatusChanged(status: Integer); cdecl;//Deprecated
  end;
  TJGnssNavigationMessage_Callback = class(TJavaGenericImport<JGnssNavigationMessage_CallbackClass, JGnssNavigationMessage_Callback>) end;

  JGnssStatusClass = interface(JObjectClass)
    ['{D36F2CE1-FD1E-4F86-8CB6-87B254273BFA}']
    {class} function _GetCONSTELLATION_BEIDOU: Integer; cdecl;
    {class} function _GetCONSTELLATION_GALILEO: Integer; cdecl;
    {class} function _GetCONSTELLATION_GLONASS: Integer; cdecl;
    {class} function _GetCONSTELLATION_GPS: Integer; cdecl;
    {class} function _GetCONSTELLATION_IRNSS: Integer; cdecl;
    {class} function _GetCONSTELLATION_QZSS: Integer; cdecl;
    {class} function _GetCONSTELLATION_SBAS: Integer; cdecl;
    {class} function _GetCONSTELLATION_UNKNOWN: Integer; cdecl;
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CONSTELLATION_BEIDOU: Integer read _GetCONSTELLATION_BEIDOU;
    {class} property CONSTELLATION_GALILEO: Integer read _GetCONSTELLATION_GALILEO;
    {class} property CONSTELLATION_GLONASS: Integer read _GetCONSTELLATION_GLONASS;
    {class} property CONSTELLATION_GPS: Integer read _GetCONSTELLATION_GPS;
    {class} property CONSTELLATION_IRNSS: Integer read _GetCONSTELLATION_IRNSS;
    {class} property CONSTELLATION_QZSS: Integer read _GetCONSTELLATION_QZSS;
    {class} property CONSTELLATION_SBAS: Integer read _GetCONSTELLATION_SBAS;
    {class} property CONSTELLATION_UNKNOWN: Integer read _GetCONSTELLATION_UNKNOWN;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/location/GnssStatus')]
  JGnssStatus = interface(JObject)
    ['{7578D1B1-992E-4F52-B963-31559E825D5C}']
    function describeContents: Integer; cdecl;
    function equals(o: JObject): Boolean; cdecl;
    function getAzimuthDegrees(satelliteIndex: Integer): Single; cdecl;
    function getBasebandCn0DbHz(satelliteIndex: Integer): Single; cdecl;
    function getCarrierFrequencyHz(satelliteIndex: Integer): Single; cdecl;
    function getCn0DbHz(satelliteIndex: Integer): Single; cdecl;
    function getConstellationType(satelliteIndex: Integer): Integer; cdecl;
    function getElevationDegrees(satelliteIndex: Integer): Single; cdecl;
    function getSatelliteCount: Integer; cdecl;
    function getSvid(satelliteIndex: Integer): Integer; cdecl;
    function hasAlmanacData(satelliteIndex: Integer): Boolean; cdecl;
    function hasBasebandCn0DbHz(satelliteIndex: Integer): Boolean; cdecl;
    function hasCarrierFrequencyHz(satelliteIndex: Integer): Boolean; cdecl;
    function hasEphemerisData(satelliteIndex: Integer): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function usedInFix(satelliteIndex: Integer): Boolean; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJGnssStatus = class(TJavaGenericImport<JGnssStatusClass, JGnssStatus>) end;

  JGnssStatus_BuilderClass = interface(JObjectClass)
    ['{96EEA07C-5BFA-419A-AEDC-D5993A556051}']
    {class} function init: JGnssStatus_Builder; cdecl;
  end;

  [JavaSignature('android/location/GnssStatus$Builder')]
  JGnssStatus_Builder = interface(JObject)
    ['{D00CE90B-52DC-49A9-9867-E1058F893ED4}']
    function addSatellite(constellationType: Integer; svid: Integer; cn0DbHz: Single; elevation: Single; azimuth: Single; hasEphemeris: Boolean; hasAlmanac: Boolean; usedInFix: Boolean; hasCarrierFrequency: Boolean; carrierFrequency: Single; hasBasebandCn0DbHz: Boolean; basebandCn0DbHz: Single): JGnssStatus_Builder; cdecl;
    function build: JGnssStatus; cdecl;
    function clearSatellites: JGnssStatus_Builder; cdecl;
  end;
  TJGnssStatus_Builder = class(TJavaGenericImport<JGnssStatus_BuilderClass, JGnssStatus_Builder>) end;

  JGnssStatus_CallbackClass = interface(JObjectClass)
    ['{9EDE45E7-C6E4-4CCD-B587-13299E1D4E50}']
    {class} function init: JGnssStatus_Callback; cdecl;
  end;

  [JavaSignature('android/location/GnssStatus$Callback')]
  JGnssStatus_Callback = interface(JObject)
    ['{1EFDADBF-4B94-4A50-8671-354EE671CFCB}']
    procedure onFirstFix(ttffMillis: Integer); cdecl;
    procedure onSatelliteStatusChanged(status: JGnssStatus); cdecl;
    procedure onStarted; cdecl;
    procedure onStopped; cdecl;
  end;
  TJGnssStatus_Callback = class(TJavaGenericImport<JGnssStatus_CallbackClass, JGnssStatus_Callback>) end;

  JGpsSatelliteClass = interface(JObjectClass)
    ['{8B2B813E-3F20-40F5-AAFF-CA38016DBF99}']
  end;

  [JavaSignature('android/location/GpsSatellite')]
  JGpsSatellite = interface(JObject)
    ['{23BDBF98-46F8-46EE-93FB-07D1EB30C7F7}']
    function getAzimuth: Single; cdecl;
    function getElevation: Single; cdecl;
    function getPrn: Integer; cdecl;
    function getSnr: Single; cdecl;
    function hasAlmanac: Boolean; cdecl;
    function hasEphemeris: Boolean; cdecl;
    function usedInFix: Boolean; cdecl;
  end;
  TJGpsSatellite = class(TJavaGenericImport<JGpsSatelliteClass, JGpsSatellite>) end;

  JGpsStatusClass = interface(JObjectClass)
    ['{875EC1CA-A26D-41DD-83C3-4C361DE1EA50}']
    {class} function _GetGPS_EVENT_FIRST_FIX: Integer; cdecl;
    {class} function _GetGPS_EVENT_SATELLITE_STATUS: Integer; cdecl;
    {class} function _GetGPS_EVENT_STARTED: Integer; cdecl;
    {class} function _GetGPS_EVENT_STOPPED: Integer; cdecl;
    {class} function create(gnssStatus: JGnssStatus; timeToFirstFix: Integer): JGpsStatus; cdecl;
    {class} property GPS_EVENT_FIRST_FIX: Integer read _GetGPS_EVENT_FIRST_FIX;
    {class} property GPS_EVENT_SATELLITE_STATUS: Integer read _GetGPS_EVENT_SATELLITE_STATUS;
    {class} property GPS_EVENT_STARTED: Integer read _GetGPS_EVENT_STARTED;
    {class} property GPS_EVENT_STOPPED: Integer read _GetGPS_EVENT_STOPPED;
  end;

  [JavaSignature('android/location/GpsStatus')]
  JGpsStatus = interface(JObject)
    ['{6ED3905E-5FBC-4E7C-A363-10A07297FF5E}']
    function getMaxSatellites: Integer; cdecl;
    function getSatellites: JIterable; cdecl;
    function getTimeToFirstFix: Integer; cdecl;
  end;
  TJGpsStatus = class(TJavaGenericImport<JGpsStatusClass, JGpsStatus>) end;

  JGpsStatus_ListenerClass = interface(IJavaClass)
    ['{1B4E4734-00D1-4DD8-856F-5B15E4BA31F3}']
  end;

  [JavaSignature('android/location/GpsStatus$Listener')]
  JGpsStatus_Listener = interface(IJavaInstance)
    ['{12C48E82-7BEA-4E6E-973D-95CD0B42B3FE}']
    procedure onGpsStatusChanged(event: Integer); cdecl;
  end;
  TJGpsStatus_Listener = class(TJavaGenericImport<JGpsStatus_ListenerClass, JGpsStatus_Listener>) end;

  JGpsStatus_NmeaListenerClass = interface(IJavaClass)
    ['{19E69909-A8B8-4723-866A-49435EC8D942}']
  end;

  [JavaSignature('android/location/GpsStatus$NmeaListener')]
  JGpsStatus_NmeaListener = interface(IJavaInstance)
    ['{7CEBB6D7-85A9-40EA-9360-BE3E6124336B}']
    procedure onNmeaReceived(timestamp: Int64; nmea: JString); cdecl;
  end;
  TJGpsStatus_NmeaListener = class(TJavaGenericImport<JGpsStatus_NmeaListenerClass, JGpsStatus_NmeaListener>) end;

  JLocationClass = interface(JObjectClass)
    ['{A0BF1527-8922-4124-94AC-D2361F1EF984}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetFORMAT_DEGREES: Integer; cdecl;
    {class} function _GetFORMAT_MINUTES: Integer; cdecl;
    {class} function _GetFORMAT_SECONDS: Integer; cdecl;
    {class} function init(provider: JString): JLocation; cdecl; overload;
    {class} function init(location: JLocation): JLocation; cdecl; overload;
    {class} function convert(coordinate: Double; outputType: Integer): JString; cdecl; overload;
    {class} function convert(coordinate: JString): Double; cdecl; overload;
    {class} procedure distanceBetween(startLatitude: Double; startLongitude: Double; endLatitude: Double; endLongitude: Double; results: TJavaArray<Single>); cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property FORMAT_DEGREES: Integer read _GetFORMAT_DEGREES;
    {class} property FORMAT_MINUTES: Integer read _GetFORMAT_MINUTES;
    {class} property FORMAT_SECONDS: Integer read _GetFORMAT_SECONDS;
  end;

  [JavaSignature('android/location/Location')]
  JLocation = interface(JObject)
    ['{B6A7FB34-312C-41BC-B02D-0107E6F39395}']
    function bearingTo(dest: JLocation): Single; cdecl;
    function describeContents: Integer; cdecl;
    function distanceTo(dest: JLocation): Single; cdecl;
    procedure dump(pw: JPrinter; prefix: JString); cdecl;//Deprecated
    function equals(o: JObject): Boolean; cdecl;
    function getAccuracy: Single; cdecl;
    function getAltitude: Double; cdecl;
    function getBearing: Single; cdecl;
    function getBearingAccuracyDegrees: Single; cdecl;
    function getElapsedRealtimeAgeMillis: Int64; cdecl; overload;
    function getElapsedRealtimeAgeMillis(referenceRealtimeMs: Int64): Int64; cdecl; overload;
    function getElapsedRealtimeMillis: Int64; cdecl;
    function getElapsedRealtimeNanos: Int64; cdecl;
    function getElapsedRealtimeUncertaintyNanos: Double; cdecl;
    function getExtras: JBundle; cdecl;
    function getLatitude: Double; cdecl;
    function getLongitude: Double; cdecl;
    function getProvider: JString; cdecl;
    function getSpeed: Single; cdecl;
    function getSpeedAccuracyMetersPerSecond: Single; cdecl;
    function getTime: Int64; cdecl;
    function getVerticalAccuracyMeters: Single; cdecl;
    function hasAccuracy: Boolean; cdecl;
    function hasAltitude: Boolean; cdecl;
    function hasBearing: Boolean; cdecl;
    function hasBearingAccuracy: Boolean; cdecl;
    function hasElapsedRealtimeUncertaintyNanos: Boolean; cdecl;
    function hasSpeed: Boolean; cdecl;
    function hasSpeedAccuracy: Boolean; cdecl;
    function hasVerticalAccuracy: Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function isComplete: Boolean; cdecl;
    function isFromMockProvider: Boolean; cdecl;//Deprecated
    function isMock: Boolean; cdecl;
    procedure removeAccuracy; cdecl;
    procedure removeAltitude; cdecl;
    procedure removeBearing; cdecl;
    procedure removeBearingAccuracy; cdecl;
    procedure removeElapsedRealtimeUncertaintyNanos; cdecl;
    procedure removeSpeed; cdecl;
    procedure removeSpeedAccuracy; cdecl;
    procedure removeVerticalAccuracy; cdecl;
    procedure reset; cdecl;
    procedure &set(location: JLocation); cdecl;
    procedure setAccuracy(horizontalAccuracyMeters: Single); cdecl;
    procedure setAltitude(altitudeMeters: Double); cdecl;
    procedure setBearing(bearingDegrees: Single); cdecl;
    procedure setBearingAccuracyDegrees(bearingAccuracyDegrees: Single); cdecl;
    procedure setElapsedRealtimeNanos(elapsedRealtimeNs: Int64); cdecl;
    procedure setElapsedRealtimeUncertaintyNanos(elapsedRealtimeUncertaintyNs: Double); cdecl;
    procedure setExtras(extras: JBundle); cdecl;
    procedure setLatitude(latitudeDegrees: Double); cdecl;
    procedure setLongitude(longitudeDegrees: Double); cdecl;
    procedure setMock(mock: Boolean); cdecl;
    procedure setProvider(provider: JString); cdecl;
    procedure setSpeed(speedMetersPerSecond: Single); cdecl;
    procedure setSpeedAccuracyMetersPerSecond(speedAccuracyMeterPerSecond: Single); cdecl;
    procedure setTime(timeMs: Int64); cdecl;
    procedure setVerticalAccuracyMeters(altitudeAccuracyMeters: Single); cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJLocation = class(TJavaGenericImport<JLocationClass, JLocation>) end;

  JLocationListenerClass = interface(IJavaClass)
    ['{C0D51209-3DC1-46D5-91C1-CC77097564DB}']
  end;

  [JavaSignature('android/location/LocationListener')]
  JLocationListener = interface(IJavaInstance)
    ['{D1CF3FB5-3BCB-4959-98D7-BD4D8F93D839}']
    procedure onFlushComplete(requestCode: Integer); cdecl;
    procedure onLocationChanged(location: JLocation); cdecl; overload;
    procedure onLocationChanged(locations: JList); cdecl; overload;
    procedure onProviderDisabled(provider: JString); cdecl;
    procedure onProviderEnabled(provider: JString); cdecl;
    procedure onStatusChanged(provider: JString; status: Integer; extras: JBundle); cdecl;//Deprecated
  end;
  TJLocationListener = class(TJavaGenericImport<JLocationListenerClass, JLocationListener>) end;

  JLocationManagerClass = interface(JObjectClass)
    ['{031AE82C-2EEE-41E4-94F8-24C4D7059246}']
    {class} function _GetACTION_GNSS_CAPABILITIES_CHANGED: JString; cdecl;
    {class} function _GetEXTRA_GNSS_CAPABILITIES: JString; cdecl;
    {class} function _GetEXTRA_LOCATION_ENABLED: JString; cdecl;
    {class} function _GetEXTRA_PROVIDER_ENABLED: JString; cdecl;
    {class} function _GetEXTRA_PROVIDER_NAME: JString; cdecl;
    {class} function _GetFUSED_PROVIDER: JString; cdecl;
    {class} function _GetGPS_PROVIDER: JString; cdecl;
    {class} function _GetKEY_FLUSH_COMPLETE: JString; cdecl;
    {class} function _GetKEY_LOCATIONS: JString; cdecl;
    {class} function _GetKEY_LOCATION_CHANGED: JString; cdecl;
    {class} function _GetKEY_PROVIDER_ENABLED: JString; cdecl;
    {class} function _GetKEY_PROXIMITY_ENTERING: JString; cdecl;
    {class} function _GetKEY_STATUS_CHANGED: JString; cdecl;
    {class} function _GetMODE_CHANGED_ACTION: JString; cdecl;
    {class} function _GetNETWORK_PROVIDER: JString; cdecl;
    {class} function _GetPASSIVE_PROVIDER: JString; cdecl;
    {class} function _GetPROVIDERS_CHANGED_ACTION: JString; cdecl;
    {class} property ACTION_GNSS_CAPABILITIES_CHANGED: JString read _GetACTION_GNSS_CAPABILITIES_CHANGED;
    {class} property EXTRA_GNSS_CAPABILITIES: JString read _GetEXTRA_GNSS_CAPABILITIES;
    {class} property EXTRA_LOCATION_ENABLED: JString read _GetEXTRA_LOCATION_ENABLED;
    {class} property EXTRA_PROVIDER_ENABLED: JString read _GetEXTRA_PROVIDER_ENABLED;
    {class} property EXTRA_PROVIDER_NAME: JString read _GetEXTRA_PROVIDER_NAME;
    {class} property FUSED_PROVIDER: JString read _GetFUSED_PROVIDER;
    {class} property GPS_PROVIDER: JString read _GetGPS_PROVIDER;
    {class} property KEY_FLUSH_COMPLETE: JString read _GetKEY_FLUSH_COMPLETE;
    {class} property KEY_LOCATIONS: JString read _GetKEY_LOCATIONS;
    {class} property KEY_LOCATION_CHANGED: JString read _GetKEY_LOCATION_CHANGED;
    {class} property KEY_PROVIDER_ENABLED: JString read _GetKEY_PROVIDER_ENABLED;
    {class} property KEY_PROXIMITY_ENTERING: JString read _GetKEY_PROXIMITY_ENTERING;
    {class} property KEY_STATUS_CHANGED: JString read _GetKEY_STATUS_CHANGED;
    {class} property MODE_CHANGED_ACTION: JString read _GetMODE_CHANGED_ACTION;
    {class} property NETWORK_PROVIDER: JString read _GetNETWORK_PROVIDER;
    {class} property PASSIVE_PROVIDER: JString read _GetPASSIVE_PROVIDER;
    {class} property PROVIDERS_CHANGED_ACTION: JString read _GetPROVIDERS_CHANGED_ACTION;
  end;

  [JavaSignature('android/location/LocationManager')]
  JLocationManager = interface(JObject)
    ['{42C3E256-9370-43B5-93E3-EDF5CE51FB28}']
    function addGpsStatusListener(listener: JGpsStatus_Listener): Boolean; cdecl;//Deprecated
    function addNmeaListener(listener: JGpsStatus_NmeaListener): Boolean; cdecl; overload;//Deprecated
    function addNmeaListener(listener: JOnNmeaMessageListener): Boolean; cdecl; overload;//Deprecated
    function addNmeaListener(listener: JOnNmeaMessageListener; handler: JHandler): Boolean; cdecl; overload;
    function addNmeaListener(executor: JExecutor; listener: JOnNmeaMessageListener): Boolean; cdecl; overload;
    procedure addProximityAlert(latitude: Double; longitude: Double; radius: Single; expiration: Int64; pendingIntent: JPendingIntent); cdecl;
    procedure addTestProvider(provider: JString; requiresNetwork: Boolean; requiresSatellite: Boolean; requiresCell: Boolean; hasMonetaryCost: Boolean; supportsAltitude: Boolean; supportsSpeed: Boolean; supportsBearing: Boolean; powerUsage: Integer; accuracy: Integer); cdecl; overload;
    procedure addTestProvider(provider: JString; properties: JProviderProperties); cdecl; overload;
    procedure addTestProvider(provider: JString; properties: JProviderProperties; extraAttributionTags: JSet); cdecl; overload;
    procedure clearTestProviderEnabled(provider: JString); cdecl;//Deprecated
    procedure clearTestProviderLocation(provider: JString); cdecl;//Deprecated
    procedure clearTestProviderStatus(provider: JString); cdecl;//Deprecated
    function getAllProviders: JList; cdecl;
    function getBestProvider(criteria: JCriteria; enabledOnly: Boolean): JString; cdecl;
    procedure getCurrentLocation(provider: JString; cancellationSignal: JCancellationSignal; executor: JExecutor; consumer: JConsumer); cdecl; overload;
    procedure getCurrentLocation(provider: JString; locationRequest: Jlocation_LocationRequest; cancellationSignal: JCancellationSignal; executor: JExecutor; consumer: JConsumer); cdecl; overload;
    function getGnssAntennaInfos: JList; cdecl;
    function getGnssCapabilities: JGnssCapabilities; cdecl;
    function getGnssHardwareModelName: JString; cdecl;
    function getGnssYearOfHardware: Integer; cdecl;
    function getGpsStatus(status: JGpsStatus): JGpsStatus; cdecl;//Deprecated
    function getLastKnownLocation(provider: JString): JLocation; cdecl;
    function getProvider(provider: JString): JLocationProvider; cdecl;//Deprecated
    function getProviderProperties(provider: JString): JProviderProperties; cdecl;
    function getProviders(enabledOnly: Boolean): JList; cdecl; overload;
    function getProviders(criteria: JCriteria; enabledOnly: Boolean): JList; cdecl; overload;
    function hasProvider(provider: JString): Boolean; cdecl;
    function isLocationEnabled: Boolean; cdecl;
    function isProviderEnabled(provider: JString): Boolean; cdecl;
    function registerAntennaInfoListener(executor: JExecutor; listener: JGnssAntennaInfo_Listener): Boolean; cdecl;
    function registerGnssMeasurementsCallback(callback: JGnssMeasurementsEvent_Callback): Boolean; cdecl; overload;//Deprecated
    function registerGnssMeasurementsCallback(callback: JGnssMeasurementsEvent_Callback; handler: JHandler): Boolean; cdecl; overload;
    function registerGnssMeasurementsCallback(executor: JExecutor; callback: JGnssMeasurementsEvent_Callback): Boolean; cdecl; overload;
    function registerGnssMeasurementsCallback(request: JGnssMeasurementRequest; executor: JExecutor; callback: JGnssMeasurementsEvent_Callback): Boolean; cdecl; overload;
    function registerGnssNavigationMessageCallback(callback: JGnssNavigationMessage_Callback): Boolean; cdecl; overload;//Deprecated
    function registerGnssNavigationMessageCallback(callback: JGnssNavigationMessage_Callback; handler: JHandler): Boolean; cdecl; overload;
    function registerGnssNavigationMessageCallback(executor: JExecutor; callback: JGnssNavigationMessage_Callback): Boolean; cdecl; overload;
    function registerGnssStatusCallback(callback: JGnssStatus_Callback): Boolean; cdecl; overload;//Deprecated
    function registerGnssStatusCallback(callback: JGnssStatus_Callback; handler: JHandler): Boolean; cdecl; overload;
    function registerGnssStatusCallback(executor: JExecutor; callback: JGnssStatus_Callback): Boolean; cdecl; overload;
    procedure removeGpsStatusListener(listener: JGpsStatus_Listener); cdecl;//Deprecated
    procedure removeNmeaListener(listener: JGpsStatus_NmeaListener); cdecl; overload;//Deprecated
    procedure removeNmeaListener(listener: JOnNmeaMessageListener); cdecl; overload;
    procedure removeProximityAlert(intent: JPendingIntent); cdecl;
    procedure removeTestProvider(provider: JString); cdecl;
    procedure removeUpdates(listener: JLocationListener); cdecl; overload;
    procedure removeUpdates(pendingIntent: JPendingIntent); cdecl; overload;
    procedure requestFlush(provider: JString; listener: JLocationListener; requestCode: Integer); cdecl; overload;
    procedure requestFlush(provider: JString; pendingIntent: JPendingIntent; requestCode: Integer); cdecl; overload;
    procedure requestLocationUpdates(provider: JString; minTimeMs: Int64; minDistanceM: Single; listener: JLocationListener); cdecl; overload;
    procedure requestLocationUpdates(provider: JString; minTimeMs: Int64; minDistanceM: Single; listener: JLocationListener; looper: JLooper); cdecl; overload;
    procedure requestLocationUpdates(provider: JString; minTimeMs: Int64; minDistanceM: Single; executor: JExecutor; listener: JLocationListener); cdecl; overload;
    procedure requestLocationUpdates(minTimeMs: Int64; minDistanceM: Single; criteria: JCriteria; listener: JLocationListener; looper: JLooper); cdecl; overload;//Deprecated
    procedure requestLocationUpdates(minTimeMs: Int64; minDistanceM: Single; criteria: JCriteria; executor: JExecutor; listener: JLocationListener); cdecl; overload;//Deprecated
    procedure requestLocationUpdates(provider: JString; minTimeMs: Int64; minDistanceM: Single; pendingIntent: JPendingIntent); cdecl; overload;
    procedure requestLocationUpdates(minTimeMs: Int64; minDistanceM: Single; criteria: JCriteria; pendingIntent: JPendingIntent); cdecl; overload;//Deprecated
    procedure requestLocationUpdates(provider: JString; locationRequest: Jlocation_LocationRequest; executor: JExecutor; listener: JLocationListener); cdecl; overload;
    procedure requestLocationUpdates(provider: JString; locationRequest: Jlocation_LocationRequest; pendingIntent: JPendingIntent); cdecl; overload;
    procedure requestSingleUpdate(provider: JString; listener: JLocationListener; looper: JLooper); cdecl; overload;//Deprecated
    procedure requestSingleUpdate(criteria: JCriteria; listener: JLocationListener; looper: JLooper); cdecl; overload;//Deprecated
    procedure requestSingleUpdate(provider: JString; pendingIntent: JPendingIntent); cdecl; overload;//Deprecated
    procedure requestSingleUpdate(criteria: JCriteria; pendingIntent: JPendingIntent); cdecl; overload;//Deprecated
    function sendExtraCommand(provider: JString; command: JString; extras: JBundle): Boolean; cdecl;
    procedure setTestProviderEnabled(provider: JString; enabled: Boolean); cdecl;
    procedure setTestProviderLocation(provider: JString; location: JLocation); cdecl;
    procedure setTestProviderStatus(provider: JString; status: Integer; extras: JBundle; updateTime: Int64); cdecl;//Deprecated
    procedure unregisterAntennaInfoListener(listener: JGnssAntennaInfo_Listener); cdecl;
    procedure unregisterGnssMeasurementsCallback(callback: JGnssMeasurementsEvent_Callback); cdecl;
    procedure unregisterGnssNavigationMessageCallback(callback: JGnssNavigationMessage_Callback); cdecl;
    procedure unregisterGnssStatusCallback(callback: JGnssStatus_Callback); cdecl;
  end;
  TJLocationManager = class(TJavaGenericImport<JLocationManagerClass, JLocationManager>) end;

  JLocationProviderClass = interface(JObjectClass)
    ['{45951F9E-53D1-428F-9B17-954C136F0DAA}']
    {class} function _GetAVAILABLE: Integer; cdecl;
    {class} function _GetOUT_OF_SERVICE: Integer; cdecl;
    {class} function _GetTEMPORARILY_UNAVAILABLE: Integer; cdecl;
    {class} property AVAILABLE: Integer read _GetAVAILABLE;
    {class} property OUT_OF_SERVICE: Integer read _GetOUT_OF_SERVICE;
    {class} property TEMPORARILY_UNAVAILABLE: Integer read _GetTEMPORARILY_UNAVAILABLE;
  end;

  [JavaSignature('android/location/LocationProvider')]
  JLocationProvider = interface(JObject)
    ['{1A055914-E219-4691-A746-CBA8DB4BA34F}']
    function getAccuracy: Integer; cdecl;
    function getName: JString; cdecl;
    function getPowerRequirement: Integer; cdecl;
    function hasMonetaryCost: Boolean; cdecl;
    function meetsCriteria(criteria: JCriteria): Boolean; cdecl;
    function requiresCell: Boolean; cdecl;
    function requiresNetwork: Boolean; cdecl;
    function requiresSatellite: Boolean; cdecl;
    function supportsAltitude: Boolean; cdecl;
    function supportsBearing: Boolean; cdecl;
    function supportsSpeed: Boolean; cdecl;
  end;
  TJLocationProvider = class(TJavaGenericImport<JLocationProviderClass, JLocationProvider>) end;

  Jlocation_LocationRequestClass = interface(JObjectClass)
    ['{2D2A7CDB-D8C0-4C4F-B4A7-7C8BC094BC90}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetPASSIVE_INTERVAL: Int64; cdecl;
    {class} function _GetQUALITY_BALANCED_POWER_ACCURACY: Integer; cdecl;
    {class} function _GetQUALITY_HIGH_ACCURACY: Integer; cdecl;
    {class} function _GetQUALITY_LOW_POWER: Integer; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property PASSIVE_INTERVAL: Int64 read _GetPASSIVE_INTERVAL;
    {class} property QUALITY_BALANCED_POWER_ACCURACY: Integer read _GetQUALITY_BALANCED_POWER_ACCURACY;
    {class} property QUALITY_HIGH_ACCURACY: Integer read _GetQUALITY_HIGH_ACCURACY;
    {class} property QUALITY_LOW_POWER: Integer read _GetQUALITY_LOW_POWER;
  end;

  [JavaSignature('android/location/LocationRequest')]
  Jlocation_LocationRequest = interface(JObject)
    ['{D82F2B59-53DB-4347-9891-837EDE06254E}']
    function describeContents: Integer; cdecl;
    function equals(o: JObject): Boolean; cdecl;
    function getDurationMillis: Int64; cdecl;
    function getIntervalMillis: Int64; cdecl;
    function getMaxUpdateDelayMillis: Int64; cdecl;
    function getMaxUpdates: Integer; cdecl;
    function getMinUpdateDistanceMeters: Single; cdecl;
    function getMinUpdateIntervalMillis: Int64; cdecl;
    function getQuality: Integer; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJlocation_LocationRequest = class(TJavaGenericImport<Jlocation_LocationRequestClass, Jlocation_LocationRequest>) end;

  JLocationRequest_BuilderClass = interface(JObjectClass)
    ['{3E314429-B1B2-4B9B-88A5-E48E43708973}']
    {class} function init(intervalMillis: Int64): JLocationRequest_Builder; cdecl; overload;
    {class} function init(locationRequest: Jlocation_LocationRequest): JLocationRequest_Builder; cdecl; overload;
  end;

  [JavaSignature('android/location/LocationRequest$Builder')]
  JLocationRequest_Builder = interface(JObject)
    ['{8650C421-3770-4DD6-98E9-458122DCA510}']
    function build: Jlocation_LocationRequest; cdecl;
    function clearMinUpdateIntervalMillis: JLocationRequest_Builder; cdecl;
    function setDurationMillis(durationMillis: Int64): JLocationRequest_Builder; cdecl;
    function setIntervalMillis(intervalMillis: Int64): JLocationRequest_Builder; cdecl;
    function setMaxUpdateDelayMillis(maxUpdateDelayMillis: Int64): JLocationRequest_Builder; cdecl;
    function setMaxUpdates(maxUpdates: Integer): JLocationRequest_Builder; cdecl;
    function setMinUpdateDistanceMeters(minUpdateDistanceMeters: Single): JLocationRequest_Builder; cdecl;
    function setMinUpdateIntervalMillis(minUpdateIntervalMillis: Int64): JLocationRequest_Builder; cdecl;
    function setQuality(quality: Integer): JLocationRequest_Builder; cdecl;
  end;
  TJLocationRequest_Builder = class(TJavaGenericImport<JLocationRequest_BuilderClass, JLocationRequest_Builder>) end;

  JOnNmeaMessageListenerClass = interface(IJavaClass)
    ['{CDE9418B-4BB8-4819-ABE5-27E054B8BFE3}']
  end;

  [JavaSignature('android/location/OnNmeaMessageListener')]
  JOnNmeaMessageListener = interface(IJavaInstance)
    ['{F8E42741-3C9D-4D80-8266-C110606E79E1}']
    procedure onNmeaMessage(message: JString; timestamp: Int64); cdecl;
  end;
  TJOnNmeaMessageListener = class(TJavaGenericImport<JOnNmeaMessageListenerClass, JOnNmeaMessageListener>) end;

  JSettingInjectorServiceClass = interface(JServiceClass)
    ['{20191F52-1C01-4EBD-9587-C4D13416FD3A}']
    {class} function _GetACTION_INJECTED_SETTING_CHANGED: JString; cdecl;
    {class} function _GetACTION_SERVICE_INTENT: JString; cdecl;
    {class} function _GetATTRIBUTES_NAME: JString; cdecl;
    {class} function _GetMETA_DATA_NAME: JString; cdecl;
    {class} function init(name: JString): JSettingInjectorService; cdecl;
    {class} procedure refreshSettings(context: JContext); cdecl;
    {class} property ACTION_INJECTED_SETTING_CHANGED: JString read _GetACTION_INJECTED_SETTING_CHANGED;
    {class} property ACTION_SERVICE_INTENT: JString read _GetACTION_SERVICE_INTENT;
    {class} property ATTRIBUTES_NAME: JString read _GetATTRIBUTES_NAME;
    {class} property META_DATA_NAME: JString read _GetMETA_DATA_NAME;
  end;

  [JavaSignature('android/location/SettingInjectorService')]
  JSettingInjectorService = interface(JService)
    ['{739CA798-301B-4FC1-A08A-39CE41189EEA}']
    function onBind(intent: JIntent): JIBinder; cdecl;
    procedure onStart(intent: JIntent; startId: Integer); cdecl;
    function onStartCommand(intent: JIntent; flags: Integer; startId: Integer): Integer; cdecl;
  end;
  TJSettingInjectorService = class(TJavaGenericImport<JSettingInjectorServiceClass, JSettingInjectorService>) end;

  JProviderPropertiesClass = interface(JObjectClass)
    ['{37A451FF-0D45-408A-B43D-DC8C4372D3FB}']
    {class} function _GetACCURACY_COARSE: Integer; cdecl;
    {class} function _GetACCURACY_FINE: Integer; cdecl;
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetPOWER_USAGE_HIGH: Integer; cdecl;
    {class} function _GetPOWER_USAGE_LOW: Integer; cdecl;
    {class} function _GetPOWER_USAGE_MEDIUM: Integer; cdecl;
    {class} property ACCURACY_COARSE: Integer read _GetACCURACY_COARSE;
    {class} property ACCURACY_FINE: Integer read _GetACCURACY_FINE;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property POWER_USAGE_HIGH: Integer read _GetPOWER_USAGE_HIGH;
    {class} property POWER_USAGE_LOW: Integer read _GetPOWER_USAGE_LOW;
    {class} property POWER_USAGE_MEDIUM: Integer read _GetPOWER_USAGE_MEDIUM;
  end;

  [JavaSignature('android/location/provider/ProviderProperties')]
  JProviderProperties = interface(JObject)
    ['{6ED9D9ED-FA34-4BC0-89D7-AC7DAAB9B920}']
    function describeContents: Integer; cdecl;
    function equals(o: JObject): Boolean; cdecl;
    function getAccuracy: Integer; cdecl;
    function getPowerUsage: Integer; cdecl;
    function hasAltitudeSupport: Boolean; cdecl;
    function hasBearingSupport: Boolean; cdecl;
    function hasCellRequirement: Boolean; cdecl;
    function hasMonetaryCost: Boolean; cdecl;
    function hasNetworkRequirement: Boolean; cdecl;
    function hasSatelliteRequirement: Boolean; cdecl;
    function hasSpeedSupport: Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJProviderProperties = class(TJavaGenericImport<JProviderPropertiesClass, JProviderProperties>) end;

  JProviderProperties_BuilderClass = interface(JObjectClass)
    ['{232E701A-0B1A-423C-910D-892D08778807}']
    {class} function init: JProviderProperties_Builder; cdecl; overload;
    {class} function init(providerProperties: JProviderProperties): JProviderProperties_Builder; cdecl; overload;
  end;

  [JavaSignature('android/location/provider/ProviderProperties$Builder')]
  JProviderProperties_Builder = interface(JObject)
    ['{87E79204-B6AF-47B9-86F9-EA668D7F2BCA}']
    function build: JProviderProperties; cdecl;
    function setAccuracy(accuracy: Integer): JProviderProperties_Builder; cdecl;
    function setHasAltitudeSupport(supportsAltitude: Boolean): JProviderProperties_Builder; cdecl;
    function setHasBearingSupport(supportsBearing: Boolean): JProviderProperties_Builder; cdecl;
    function setHasCellRequirement(requiresCell: Boolean): JProviderProperties_Builder; cdecl;
    function setHasMonetaryCost(monetaryCost: Boolean): JProviderProperties_Builder; cdecl;
    function setHasNetworkRequirement(requiresNetwork: Boolean): JProviderProperties_Builder; cdecl;
    function setHasSatelliteRequirement(requiresSatellite: Boolean): JProviderProperties_Builder; cdecl;
    function setHasSpeedSupport(supportsSpeed: Boolean): JProviderProperties_Builder; cdecl;
    function setPowerUsage(powerUsage: Integer): JProviderProperties_Builder; cdecl;
  end;
  TJProviderProperties_Builder = class(TJavaGenericImport<JProviderProperties_BuilderClass, JProviderProperties_Builder>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Androidapi.JNI.Location.JAddress', TypeInfo(Androidapi.JNI.Location.JAddress));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JCriteria', TypeInfo(Androidapi.JNI.Location.JCriteria));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGeocoder', TypeInfo(Androidapi.JNI.Location.JGeocoder));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGeocoder_GeocodeListener', TypeInfo(Androidapi.JNI.Location.JGeocoder_GeocodeListener));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssAntennaInfo', TypeInfo(Androidapi.JNI.Location.JGnssAntennaInfo));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssAntennaInfo_Builder', TypeInfo(Androidapi.JNI.Location.JGnssAntennaInfo_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssAntennaInfo_Listener', TypeInfo(Androidapi.JNI.Location.JGnssAntennaInfo_Listener));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssAntennaInfo_PhaseCenterOffset', TypeInfo(Androidapi.JNI.Location.JGnssAntennaInfo_PhaseCenterOffset));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssAntennaInfo_SphericalCorrections', TypeInfo(Androidapi.JNI.Location.JGnssAntennaInfo_SphericalCorrections));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssAutomaticGainControl', TypeInfo(Androidapi.JNI.Location.JGnssAutomaticGainControl));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssAutomaticGainControl_Builder', TypeInfo(Androidapi.JNI.Location.JGnssAutomaticGainControl_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssCapabilities', TypeInfo(Androidapi.JNI.Location.JGnssCapabilities));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssCapabilities_Builder', TypeInfo(Androidapi.JNI.Location.JGnssCapabilities_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssClock', TypeInfo(Androidapi.JNI.Location.JGnssClock));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssMeasurement', TypeInfo(Androidapi.JNI.Location.JGnssMeasurement));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssMeasurementRequest', TypeInfo(Androidapi.JNI.Location.JGnssMeasurementRequest));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssMeasurementRequest_Builder', TypeInfo(Androidapi.JNI.Location.JGnssMeasurementRequest_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssMeasurementsEvent', TypeInfo(Androidapi.JNI.Location.JGnssMeasurementsEvent));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssMeasurementsEvent_Builder', TypeInfo(Androidapi.JNI.Location.JGnssMeasurementsEvent_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssMeasurementsEvent_Callback', TypeInfo(Androidapi.JNI.Location.JGnssMeasurementsEvent_Callback));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssNavigationMessage', TypeInfo(Androidapi.JNI.Location.JGnssNavigationMessage));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssNavigationMessage_Callback', TypeInfo(Androidapi.JNI.Location.JGnssNavigationMessage_Callback));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssStatus', TypeInfo(Androidapi.JNI.Location.JGnssStatus));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssStatus_Builder', TypeInfo(Androidapi.JNI.Location.JGnssStatus_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGnssStatus_Callback', TypeInfo(Androidapi.JNI.Location.JGnssStatus_Callback));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGpsSatellite', TypeInfo(Androidapi.JNI.Location.JGpsSatellite));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGpsStatus', TypeInfo(Androidapi.JNI.Location.JGpsStatus));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGpsStatus_Listener', TypeInfo(Androidapi.JNI.Location.JGpsStatus_Listener));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JGpsStatus_NmeaListener', TypeInfo(Androidapi.JNI.Location.JGpsStatus_NmeaListener));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JLocation', TypeInfo(Androidapi.JNI.Location.JLocation));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JLocationListener', TypeInfo(Androidapi.JNI.Location.JLocationListener));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JLocationManager', TypeInfo(Androidapi.JNI.Location.JLocationManager));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JLocationProvider', TypeInfo(Androidapi.JNI.Location.JLocationProvider));
  TRegTypes.RegisterType('Androidapi.JNI.Location.Jlocation_LocationRequest', TypeInfo(Androidapi.JNI.Location.Jlocation_LocationRequest));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JLocationRequest_Builder', TypeInfo(Androidapi.JNI.Location.JLocationRequest_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JOnNmeaMessageListener', TypeInfo(Androidapi.JNI.Location.JOnNmeaMessageListener));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JSettingInjectorService', TypeInfo(Androidapi.JNI.Location.JSettingInjectorService));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JProviderProperties', TypeInfo(Androidapi.JNI.Location.JProviderProperties));
  TRegTypes.RegisterType('Androidapi.JNI.Location.JProviderProperties_Builder', TypeInfo(Androidapi.JNI.Location.JProviderProperties_Builder));
end;

initialization
  RegisterTypes;
end.


