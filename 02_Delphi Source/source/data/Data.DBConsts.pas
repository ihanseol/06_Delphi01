{*******************************************************}
{                                                       }
{                Delphi Runtime Library                 }
{                                                       }
{ Copyright(c) 1995-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Data.DBConsts;

interface

resourcestring
  SInvalidFieldSize = 'Invalid field size';
  SInvalidFieldKind = 'Invalid FieldKind';
  SInvalidFieldRegistration = 'Invalid field registration';
  SUnknownFieldType = 'Field ''%s'' is of an unknown type';
  SFieldNameMissing = 'Field name missing';
  SDuplicateFieldName = 'Duplicate field name ''%s''';
  SFieldNotFound = 'Field ''%s'' not found';
  SFieldAccessError = 'Cannot access field ''%s'' as type %s';
  SFieldValueError = 'Invalid value for field ''%s''';
  SFieldRangeError = '%g is not a valid value for field ''%s''. The allowed range is %g to %g';
  SBcdFieldRangeError = '%s is not a valid value for field ''%s''. The allowed range is %s to %s';
  SInvalidIntegerValue = '''%s'' is not a valid integer value for field ''%s''';
  SInvalidBoolValue = '''%s'' is not a valid boolean value for field ''%s''';
  SInvalidFloatValue = '''%s'' is not a valid floating point value for field ''%s''';
  SFieldTypeMismatch = 'Type mismatch for field ''%s'', expecting: %s actual: %s';
  SFieldSizeMismatch = 'Size mismatch for field ''%s'', expecting: %d actual: %d';
  SInvalidVarByteArray = 'Invalid variant type or size for field ''%s''';
  SFieldOutOfRange = 'Value of field ''%s'' is out of range';
//  SBCDOverflow = '(Overflow)';
  SCantAdjustPrecision = 'Error adjusting BCD precision';
  SFieldRequired = 'Field ''%s'' must have a value';
  SDataSetMissing = 'Field ''%s'' has no dataset';
  SInvalidCalcType = 'Field ''%s'' cannot be a calculated or lookup field';
  SFieldReadOnly = 'Field ''%s'' cannot be modified';
  SFieldIndexError = 'Field index out of range';
  SNoFieldIndexes = 'No index currently active';
  SNotIndexField = 'Field ''%s'' is not indexed and cannot be modified';
  SIndexFieldMissing = 'Cannot access index field ''%s''';
  SDuplicateIndexName = 'Duplicate index name ''%s''';
  SNoIndexForFields = 'No index for fields ''%s''';
  SIndexNotFound = 'Index ''%s'' not found';
  SDBDuplicateName = 'Duplicate name ''%s'' in %s';
  SCircularDataLink = 'Circular datalinks are not allowed';
  SLookupInfoError = 'Lookup information for field ''%s'' is incomplete';
  SNewLookupFieldCaption = 'New Lookup Field';
  SDataSourceChange = 'DataSource cannot be changed';
  SNoNestedMasterSource = 'Nested datasets cannot have a MasterSource';
  SDataSetOpen = 'Cannot perform this operation on an open dataset';
  SNotEditing = 'Dataset not in edit or insert mode';
  SDataSetClosed = 'Cannot perform this operation on a closed dataset';
  SDataSetEmpty = 'Cannot perform this operation on an empty dataset';
  SDataSetReadOnly = 'Cannot modify a read-only dataset';
  SNestedDataSetClass = 'Nested dataset must inherit from %s';
  SExprTermination = 'Filter expression incorrectly terminated';
  SExprNameError = 'Unterminated field name';
  SExprStringError = 'Unterminated string constant';
  SExprInvalidChar = 'Invalid filter expression character: ''%s''';
  SExprNoLParen = '''('' expected but %s found';
  SExprNoRParen = ''')'' expected but %s found';
  SExprNoRParenOrComma = ''')'' or '','' expected but %s found';
  SExprExpected = 'Expression expected but %s found';
  SExprBadField = 'Field ''%s'' cannot be used in a filter expression';
  SExprBadNullTest = 'NULL only allowed with ''='' and ''<>''';
  SExprRangeError = 'Constant out of range';
  SExprNotBoolean = 'Field ''%s'' is not of type Boolean';
  SExprIncorrect = 'Incorrectly formed filter expression';
  SExprNothing = 'nothing';
  SExprTypeMis = 'Type mismatch in expression';
  SExprBadScope = 'Operation cannot mix aggregate value with record-varying value';
  SExprNoArith = 'Arithmetic in filter expressions not supported';
  SExprNotAgg = 'Expression is not an aggregate expression';
  SExprBadConst = 'Constant is not correct type %s';
  SExprNoAggFilter = 'Aggregate expressions not allowed in filters';
  SExprEmptyInList = 'IN predicate list may not be empty';
  SInvalidKeywordUse = 'Invalid use of keyword';
  STextFalse = 'False';
  STextTrue = 'True';
  SParameterNotFound = 'Parameter ''%s'' not found';
  SInvalidVersion = 'Unable to load bind parameters';
  SParamTooBig = 'Parameter ''%s'', cannot save data larger than %d bytes';
  SBadFieldType = 'Field ''%s'' is of an unsupported type';
  SAggActive = 'Property may not be modified while aggregate is active';
  SProviderSQLNotSupported = 'SQL not supported';
  SProviderExecuteNotSupported = 'Execute not supported';
  SExprNoAggOnCalcs = 'Field ''%s'' is not the correct type of calculated field to be used in an aggregate, use an internalcalc';
  SRecordChanged = 'Record not found or changed by another user';
  SDataSetUnidirectional = 'Operation not allowed on a unidirectional dataset';
  SUnassignedVar = 'Unassigned variant value';
  SRecordNotFound = 'Record not found';
  SFileNameBlank = 'FileName property cannot be blank';
  SFieldNameTooLarge = 'Fieldname %s exceeds %d chars';

{ For FMTBcd }

  SBcdOverflow = 'BCD overflow';
  SInvalidBcdValue = '%s is not a valid BCD value';
  SInvalidFormatType = 'Invalid format type for BCD';

{ For SqlTimSt }

  SCouldNotParseTimeStamp = 'Could not parse SQL TimeStamp string';
  SInvalidSqlTimeStamp = 'Invalid SQL date/time values';
  SCalendarTimeCannotBeRepresented = 'Calendar time cannot be represented';

{ For DBJson }

  SDBJMustBeAssigned = '%s.%s must be assigned';
  SDBJMustBeInactive = '%s.%s must be inactive';
  SDBJMustBeActive = '%s.%s must be active';
  SDBJFieldTypeMismatch = 'Field [%s] data type mismatch. Current type [%s], new type [%s]';

  SDeleteRecordQuestion = 'Delete record?';
  SDeleteMultipleRecordsQuestion = 'Delete all selected records?';
  STooManyColumns = 'Grid requested to display more than 256 columns';

  { For reconcile error }
  SSkip = 'Skip';
  SAbort = 'Abort';
  SMerge = 'Merge';
  SCorrect = 'Correct';
  SCancel  = 'Cancel';
  SRefresh = 'Refresh';
  SModified = 'Modified';
  SInserted = 'Inserted';
  SDeleted  = 'Deleted';
  SCaption = 'Update Error - %s';
  SUnchanged = '<Unchanged>';  
  SBinary = '(Binary)';                              
  SAdt = '(ADT)';   
  SArray = '(Array)'; 
  SFieldName = 'Field Name'; 
  SOriginal = 'Original Value'; 
  SConflict = 'Conflicting Value';  
  SValue = ' Value';   
  SNoData = '<No Records>';
  SNew = 'New';    

implementation

end.
