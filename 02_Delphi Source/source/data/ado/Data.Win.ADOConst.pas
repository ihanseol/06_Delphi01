{*******************************************************}
{                                                       }
{                Delphi Runtime Library                 }
{                                                       }
{ Copyright(c) 1995-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Data.Win.ADOConst;

interface

resourcestring
  SInvalidEnumValue = 'Invalid Enum Value';
  SMissingConnection = 'Missing Connection or ConnectionString';
  SNoDetailFilter = 'Filter property cannot be used for detail tables';
  SBookmarksRequired = 'Dataset does not support bookmarks, which are required for multi-record data controls'; 
  SMissingCommandText = 'Missing %s property';
  SNoResultSet = 'CommandText does not return a result set';
  SADOCreateError = 'Error creating object.  Please verify that the Microsoft Data Access Components 2.1 (or later) have been properly installed';
  SEventsNotSupported = 'Events are not supported with server side TableDirect cursors';
  SUsupportedFieldType = 'Unsupported field type (%s) in field %s';
  SNoMatchingADOType = 'No matching ADO data type for %s';
  SConnectionRequired = 'A connection component is required for async ExecuteOptions';
  SCantRequery = 'Cannot perform a requery after connection has changed';
  SNoFilterOptions = 'FilterOptions are not supported';
  SRecordsetNotOpen = 'Recordset is not open';
  sNameAttr = 'Name';
  sValueAttr = 'Value';

implementation

end.
 
