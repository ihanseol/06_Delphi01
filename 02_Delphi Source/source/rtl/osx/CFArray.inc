{*******************************************************}
{                                                       }
{              Delphi Runtime Library                   }
{                                                       }
{          File: CFArray.h                              }
{          Copyright (c) 1998-2009, Apple Inc.          }
{          All Rights Reserved.                         }
{                                                       }
{       Translator: Embarcadero Technologies, Inc.      }
{ Copyright(c) 2010-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{ @header CFArray
	CFArray implements an ordered, compact container of pointer-sized
	values. Values are accessed via integer keys (indices), from the
	range 0 to N-1, where N is the number of values in the array when
	an operation is performed. The array is said to be "compact" because
	deleted or inserted values do not leave a gap in the key space --
	the values with higher-numbered indices have their indices
	renumbered lower (or higher, in the case of insertion) so that the
	set of valid indices is always in the integer range [0, N-1]. Thus,
	the index to access a particular value in the array may change over
	time as other values are inserted into or deleted from the array.

	Arrays come in two flavors, immutable, which cannot have values
	added to them or removed from them after the array is created, and
	mutable, to which you can add values or from which remove values.
	Mutable arrays can have an unlimited number of values (or rather,
	limited only by constraints external to CFArray, like the amount
	of available memory).

	As with all CoreFoundation collection types, arrays maintain hard
	references on the values you put in them, but the retaining and
	releasing functions are user-defined callbacks that can actually do
	whatever the user wants (for example, nothing).

	Computational Complexity
	The access time for a value in the array is guaranteed to be at
	worst O(lg N) for any implementation, current and future, but will
	often be O(1) (constant time). Linear search operations similarly
	have a worst case complexity of O(N*lg N), though typically the
	bounds will be tighter, and so on. Insertion or deletion operations
	will typically be linear in the number of values in the array, but
	may be O(N*lg N) clearly in the worst case in some implementations.
	There are no favored positions within the array for performance;
	that is, it is not necessarily faster to access values with low
	indices, or to insert or delete values with high indices, or
	whatever. }

{$IF NOT DEFINED(__COREFOUNDATION_CFARRAY__)}

{$DEFINE __COREFOUNDATION_CFARRAY__}

{$I CFBase.inc}

{ !
	@typedef CFArrayCallBacks
	Structure containing the callbacks of a CFArray.
	@field version The version number of the structure type being passed
		in as a parameter to the CFArray creation functions. This
		structure is version 0.
	@field retain The callback used to add a retain for the array on
		values as they are put into the array. This callback returns
		the value to store in the array, which is usually the value
		parameter passed to this callback, but may be a different
		value if a different value should be stored in the array.
		The array's allocator is passed as the first argument.
	@field release The callback used to remove a retain previously added
		for the array from values as they are removed from the
		array. The array's allocator is passed as the first
		argument.
	@field copyDescription The callback used to create a descriptive
		string representation of each value in the array. This is
		used by the CFCopyDescription() function.
	@field equal The callback used to compare values in the array for
		equality for some operations. }

type
  CFArrayRetainCallBack = function(allocator: CFAllocatorRef; 
    value: Pointer): Pointer; cdecl;
  {$EXTERNALSYM CFArrayRetainCallBack}
  CFArrayReleaseCallBack = procedure(allocator: CFAllocatorRef; 
    value: Pointer); cdecl;
  {$EXTERNALSYM CFArrayReleaseCallBack}
  CFArrayCopyDescriptionCallBack = function(
    value: Pointer): CFStringRef; cdecl;
  {$EXTERNALSYM CFArrayCopyDescriptionCallBack}
  CFArrayEqualCallBack = function(value1: Pointer; 
    value2: Pointer): Boolean; cdecl;
  {$EXTERNALSYM CFArrayEqualCallBack}

  PCFArrayCallBacks = ^CFArrayCallBacks;
  CFArrayCallBacks = record
    version: CFIndex;
    retain: CFArrayRetainCallBack;
    release: CFArrayReleaseCallBack;
    copyDescription: CFArrayCopyDescriptionCallBack;
    equal: CFArrayEqualCallBack;
  end;
  {$EXTERNALSYM CFArrayCallBacks}

{ !
	@constant kCFTypeArrayCallBacks
	Predefined CFArrayCallBacks structure containing a set of callbacks
	appropriate for use when the values in a CFArray are all CFTypes.
 }
function kCFTypeArrayCallBacks: CFArrayCallBacks;
{$EXTERNALSYM kCFTypeArrayCallBacks}

{ !
	@typedef CFArrayApplierFunction
	Type of the callback function used by the apply functions of
		CFArrays.
	@param value The current value from the array.
	@param context The user-defined context parameter given to the apply
		function.
 }
type
  CFArrayApplierFunction = procedure(value: Pointer; context: Pointer); cdecl;
  {$EXTERNALSYM CFArrayApplierFunction}

{ !
	@typedef CFArrayRef
	This is the type of a reference to immutable CFArrays.
 }

  PCFArrayRef = ^CFArrayRef;
  CFArrayRef = ^__CFArray; 
  {$EXTERNALSYM CFArrayRef}
  __CFArray = record
  end;
  {$EXTERNALSYM __CFArray}

{ !
	@typedef CFMutableArrayRef
	This is the type of a reference to mutable CFArrays.
 }
  PCFMutableArrayRef = ^CFMutableArrayRef;
  CFMutableArrayRef = ^__CFArray; 
  {$EXTERNALSYM CFMutableArrayRef}

{ !
	@function CFArrayGetTypeID
	Returns the type identifier of all CFArray instances.
 }
function CFArrayGetTypeID: CFTypeID; cdecl;
  external CoreFoundationLib name _PU + 'CFArrayGetTypeID';
{$EXTERNALSYM CFArrayGetTypeID}

{ !
	@function CFArrayCreate
	Creates a new immutable array with the given values.
	@param allocator The CFAllocator which should be used to allocate
		memory for the array and its storage for values. This
		parameter may be NULL in which case the current default
		CFAllocator is used. If this reference is not a valid
		CFAllocator, the behavior is undefined.
	@param values A C array of the pointer-sized values to be in the
		array. The values in the array are ordered in the same order
		in which they appear in this C array. This parameter may be
		NULL if the numValues parameter is 0. This C array is not
		changed or freed by this function. If this parameter is not
		a valid pointer to a C array of at least numValues pointers,
		the behavior is undefined.
	@param numValues The number of values to copy from the values C
		array into the CFArray. This number will be the count of the
		array.
		If this parameter is negative, or greater than the number of
		values actually in the value's C array, the behavior is
		undefined.
	@param callBacks A pointer to a CFArrayCallBacks structure
		initialized with the callbacks for the array to use on each
		value in the array. The retain callback will be used within
		this function, for example, to retain all of the new values
		from the values C array. A copy of the contents of the
		callbacks structure is made, so that a pointer to a
		structure on the stack can be passed in, or can be reused
		for multiple array creations. If the version field of this
		callbacks structure is not one of the defined ones for
		CFArray, the behavior is undefined. The retain field may be
		NULL, in which case the CFArray will do nothing to add a
		retain to the contained values for the array. The release
		field may be NULL, in which case the CFArray will do nothing
		to remove the array's retain (if any) on the values when the
		array is destroyed. If the copyDescription field is NULL,
		the array will create a simple description for the value. If
		the equal field is NULL, the array will use pointer equality
		to test for equality of values. This callbacks parameter
		itself may be NULL, which is treated as if a valid structure
		of version 0 with all fields NULL had been passed in.
		Otherwise, if any of the fields are not valid pointers to
		functions of the correct type, or this parameter is not a
		valid pointer to a  CFArrayCallBacks callbacks structure,
		the behavior is undefined. If any of the values put into the
		array is not one understood by one of the callback functions
		the behavior when that callback function is used is
		undefined.
	@result A reference to the new immutable CFArray.
 }
function CFArrayCreate(allocator: CFAllocatorRef; values: Pointer;
  numValues: CFIndex; callBacks: PCFArrayCallBacks): CFArrayRef; cdecl;
  external CoreFoundationLib name _PU + 'CFArrayCreate';
{$EXTERNALSYM CFArrayCreate}

{ !
	@function CFArrayCreateCopy
	Creates a new immutable array with the values from the given array.
	@param allocator The CFAllocator which should be used to allocate
		memory for the array and its storage for values. This
		parameter may be NULL in which case the current default
		CFAllocator is used. If this reference is not a valid
		CFAllocator, the behavior is undefined.
	@param theArray The array which is to be copied. The values from the
		array are copied as pointers into the new array (that is,
		the values themselves are copied, not that which the values
		point to, if anything). However, the values are also
		retained by the new array. The count of the new array will
		be the same as the given array. The new array uses the same
		callbacks as the array to be copied. If this parameter is
		not a valid CFArray, the behavior is undefined.
	@result A reference to the new immutable CFArray.
 }
function CFArrayCreateCopy(allocator: CFAllocatorRef; theArray: CFArrayRef): CFArrayRef; cdecl;
  external CoreFoundationLib name _PU + 'CFArrayCreateCopy';
{$EXTERNALSYM CFArrayCreateCopy}

{ !
	@function CFArrayCreateMutable
	Creates a new empty mutable array.
	@param allocator The CFAllocator which should be used to allocate
		memory for the array and its storage for values. This
		parameter may be NULL in which case the current default
		CFAllocator is used. If this reference is not a valid
		CFAllocator, the behavior is undefined.
	@param capacity A hint about the number of values that will be held
		by the CFArray. Pass 0 for no hint. The implementation may
		ignore this hint, or may use it to optimize various
		operations. An array's actual capacity is only limited by
		address space and available memory constraints). If this
		parameter is negative, the behavior is undefined.
	@param callBacks A pointer to a CFArrayCallBacks structure
		initialized with the callbacks for the array to use on each
		value in the array. A copy of the contents of the
		callbacks structure is made, so that a pointer to a
		structure on the stack can be passed in, or can be reused
		for multiple array creations. If the version field of this
		callbacks structure is not one of the defined ones for
		CFArray, the behavior is undefined. The retain field may be
		NULL, in which case the CFArray will do nothing to add a
		retain to the contained values for the array. The release
		field may be NULL, in which case the CFArray will do nothing
		to remove the array's retain (if any) on the values when the
		array is destroyed. If the copyDescription field is NULL,
		the array will create a simple description for the value. If
		the equal field is NULL, the array will use pointer equality
		to test for equality of values. This callbacks parameter
		itself may be NULL, which is treated as if a valid structure
		of version 0 with all fields NULL had been passed in.
		Otherwise, if any of the fields are not valid pointers to
		functions of the correct type, or this parameter is not a
		valid pointer to a  CFArrayCallBacks callbacks structure,
		the behavior is undefined. If any of the values put into the
		array is not one understood by one of the callback functions
		the behavior when that callback function is used is
		undefined.
	@result A reference to the new mutable CFArray.
 }
function CFArrayCreateMutable(allocator: CFAllocatorRef; capacity: CFIndex;
  callBacks: PCFArrayCallBacks): CFMutableArrayRef; cdecl;
  external CoreFoundationLib name _PU + 'CFArrayCreateMutable';
{$EXTERNALSYM CFArrayCreateMutable}

{ !
	@function CFArrayCreateMutableCopy
	Creates a new mutable array with the values from the given array.
	@param allocator The CFAllocator which should be used to allocate
		memory for the array and its storage for values. This
		parameter may be NULL in which case the current default
		CFAllocator is used. If this reference is not a valid
		CFAllocator, the behavior is undefined.
        @param capacity A hint about the number of values that will be held
                by the CFArray. Pass 0 for no hint. The implementation may
                ignore this hint, or may use it to optimize various
                operations. An array's actual capacity is only limited by 
                address space and available memory constraints).
		This parameter must be greater than or equal
		to the count of the array which is to be copied, or the
		behavior is undefined. If this parameter is negative, the
		behavior is undefined.
	@param theArray The array which is to be copied. The values from the
		array are copied as pointers into the new array (that is,
		the values themselves are copied, not that which the values
		point to, if anything). However, the values are also
		retained by the new array. The count of the new array will
		be the same as the given array. The new array uses the same
		callbacks as the array to be copied. If this parameter is
		not a valid CFArray, the behavior is undefined.
	@result A reference to the new mutable CFArray.
 }
function CFArrayCreateMutableCopy(allocator: CFAllocatorRef; capacity: CFIndex;
  theArray: CFArrayRef): CFMutableArrayRef; cdecl;
  external CoreFoundationLib name _PU + 'CFArrayCreateMutableCopy';
{$EXTERNALSYM CFArrayCreateMutableCopy}

{ !
	@function CFArrayGetCount
	Returns the number of values currently in the array.
	@param theArray The array to be queried. If this parameter is not a valid
		CFArray, the behavior is undefined.
	@result The number of values in the array.
 }
function CFArrayGetCount(theArray: CFArrayRef): CFIndex; cdecl;
  external CoreFoundationLib name _PU + 'CFArrayGetCount';
{$EXTERNALSYM CFArrayGetCount}

{ @function CFArrayGetCountOfValue
	Counts the number of times the given value occurs in the array.
	@param theArray The array to be searched. If this parameter is not a
		valid CFArray, the behavior is undefined.
	@param range The range within the array to search. If the range
		location or end point (defined by the location plus length
		minus 1) is outside the index space of the array (0 to
		N-1 inclusive, where N is the count of the array), the
		behavior is undefined. If the range length is negative, the
		behavior is undefined. The range may be empty (length 0).
	@param value The value for which to find matches in the array. The
		equal() callback provided when the array was created is
		used to compare. If the equal() callback was NULL, pointer
		equality (in C, ==) is used. If value, or any of the values
		in the array, are not understood by the equal() callback,
		the behavior is undefined.
	@result The number of times the given value occurs in the array,
		within the specified range.
 }
function CFArrayGetCountOfValue(theArray: CFArrayRef; range: CFRange;
  value: Pointer): CFIndex; cdecl;
  external CoreFoundationLib name _PU + 'CFArrayGetCountOfValue';
{$EXTERNALSYM CFArrayGetCountOfValue}

{ !
	@function CFArrayContainsValue
	Reports whether or not the value is in the array.
	@param theArray The array to be searched. If this parameter is not a
		valid CFArray, the behavior is undefined.
	@param range The range within the array to search. If the range
		location or end point (defined by the location plus length
		minus 1) is outside the index space of the array (0 to
		N-1 inclusive, where N is the count of the array), the
		behavior is undefined. If the range length is negative, the
		behavior is undefined. The range may be empty (length 0).
	@param value The value for which to find matches in the array. The
		equal() callback provided when the array was created is
		used to compare. If the equal() callback was NULL, pointer
		equality (in C, ==) is used. If value, or any of the values
		in the array, are not understood by the equal() callback,
		the behavior is undefined.
	@result true, if the value is in the specified range of the array,
		otherwise false.
 }
function CFArrayContainsValue(theArray: CFArrayRef; range: CFRange;
  value: Pointer): Boolean; cdecl;
  external CoreFoundationLib name _PU + 'CFArrayContainsValue';
{$EXTERNALSYM CFArrayContainsValue}

{ !
	@function CFArrayGetValueAtIndex
	Retrieves the value at the given index.
	@param theArray The array to be queried. If this parameter is not a
		valid CFArray, the behavior is undefined.
	@param idx The index of the value to retrieve. If the index is
		outside the index space of the array (0 to N-1 inclusive,
		where N is the count of the array), the behavior is
		undefined.
	@result The value with the given index in the array.
 }
function CFArrayGetValueAtIndex(theArray: CFArrayRef; idx: CFIndex): Pointer; cdecl;
  external CoreFoundationLib name _PU + 'CFArrayGetValueAtIndex';
{$EXTERNALSYM CFArrayGetValueAtIndex}

{ !
	@function CFArrayGetValues
	Fills the buffer with values from the array.
	@param theArray The array to be queried. If this parameter is not a
		valid CFArray, the behavior is undefined.
	@param range The range of values within the array to retrieve. If
		the range location or end point (defined by the location
		plus length minus 1) is outside the index space of the
		array (0 to N-1 inclusive, where N is the count of the
		array), the behavior is undefined. If the range length is
		negative, the behavior is undefined. The range may be empty
		(length 0), in which case no values are put into the buffer.
	@param values A C array of pointer-sized values to be filled with
		values from the array. The values in the C array are ordered
		in the same order in which they appear in the array. If this
		parameter is not a valid pointer to a C array of at least
		range.length pointers, the behavior is undefined.
 }
procedure CFArrayGetValues(theArray: CFArrayRef; range: CFRange; values: Pointer); cdecl;
  external CoreFoundationLib name _PU + 'CFArrayGetValues';
{$EXTERNALSYM CFArrayGetValues}

{ !
	@function CFArrayApplyFunction
	Calls a function once for each value in the array.
	@param theArray The array to be operated upon. If this parameter is not
		a valid CFArray, the behavior is undefined.
	@param range The range of values within the array to which to apply
		the function. If the range location or end point (defined by
		the location plus length minus 1) is outside the index
		space of the array (0 to N-1 inclusive, where N is the count
		of the array), the behavior is undefined. If the range
		length is negative, the behavior is undefined. The range may
		be empty (length 0).
	@param applier The callback function to call once for each value in
		the given range in the array. If this parameter is not a
		pointer to a function of the correct prototype, the behavior
		is undefined. If there are values in the range which the
		applier function does not expect or cannot properly apply
		to, the behavior is undefined. 
	@param context A pointer-sized user-defined value, which is passed
		as the second parameter to the applier function, but is
		otherwise unused by this function. If the context is not
		what is expected by the applier function, the behavior is
		undefined.
 }
procedure CFArrayApplyFunction(theArray: CFArrayRef; range: CFRange;
  applier: CFArrayApplierFunction; context: Pointer); cdecl;
  external CoreFoundationLib name _PU + 'CFArrayApplyFunction';
{$EXTERNALSYM CFArrayApplyFunction}

{ !
	@function CFArrayGetFirstIndexOfValue
	Searches the array for the value.
	@param theArray The array to be searched. If this parameter is not a
		valid CFArray, the behavior is undefined.
	@param range The range within the array to search. If the range
		location or end point (defined by the location plus length
		minus 1) is outside the index space of the array (0 to
		N-1 inclusive, where N is the count of the array), the
		behavior is undefined. If the range length is negative, the
		behavior is undefined. The range may be empty (length 0).
		The search progresses from the smallest index defined by
		the range to the largest.
	@param value The value for which to find a match in the array. The
		equal() callback provided when the array was created is
		used to compare. If the equal() callback was NULL, pointer
		equality (in C, ==) is used. If value, or any of the values
		in the array, are not understood by the equal() callback,
		the behavior is undefined.
	@result The lowest index of the matching values in the range, or
		kCFNotFound if no value in the range matched.
 }
function CFArrayGetFirstIndexOfValue(theArray: CFArrayRef; range: CFRange;
  value: Pointer): CFIndex; cdecl;
  external CoreFoundationLib name _PU + 'CFArrayGetFirstIndexOfValue';
{$EXTERNALSYM CFArrayGetFirstIndexOfValue}

{ !
	@function CFArrayGetLastIndexOfValue
	Searches the array for the value.
	@param theArray The array to be searched. If this parameter is not a
		valid CFArray, the behavior is undefined.
	@param range The range within the array to search. If the range
		location or end point (defined by the location plus length
		minus 1) is outside the index space of the array (0 to
		N-1 inclusive, where N is the count of the array), the
		behavior is undefined. If the range length is negative, the
		behavior is undefined. The range may be empty (length 0).
		The search progresses from the largest index defined by the
		range to the smallest.
	@param value The value for which to find a match in the array. The
		equal() callback provided when the array was created is
		used to compare. If the equal() callback was NULL, pointer
		equality (in C, ==) is used. If value, or any of the values
		in the array, are not understood by the equal() callback,
		the behavior is undefined.
	@result The highest index of the matching values in the range, or
		kCFNotFound if no value in the range matched.
 }
function CFArrayGetLastIndexOfValue(theArray: CFArrayRef; range: CFRange;
  value: Pointer): CFIndex; cdecl;
  external CoreFoundationLib name _PU + 'CFArrayGetLastIndexOfValue';
{$EXTERNALSYM CFArrayGetLastIndexOfValue}

{ !
	@function CFArrayBSearchValues
	Searches the array for the value using a binary search algorithm.
	@param theArray The array to be searched. If this parameter is not a
		valid CFArray, the behavior is undefined. If the array is
		not sorted from least to greatest according to the
		comparator function, the behavior is undefined.
	@param range The range within the array to search. If the range
		location or end point (defined by the location plus length
		minus 1) is outside the index space of the array (0 to
		N-1 inclusive, where N is the count of the array), the
		behavior is undefined. If the range length is negative, the
		behavior is undefined. The range may be empty (length 0).
	@param value The value for which to find a match in the array. If
		value, or any of the values in the array, are not understood
		by the comparator callback, the behavior is undefined.
	@param comparator The function with the comparator function type
		signature which is used in the binary search operation to
		compare values in the array with the given value. If this
		parameter is not a pointer to a function of the correct
		prototype, the behavior is undefined. If there are values
		in the range which the comparator function does not expect
		or cannot properly compare, the behavior is undefined.
	@param context A pointer-sized user-defined value, which is passed
		as the third parameter to the comparator function, but is
		otherwise unused by this function. If the context is not
		what is expected by the comparator function, the behavior is
		undefined.
	@result The return value is either 1) the index of a value that
		matched, if the target value matches one or more in the
		range, 2) greater than or equal to the end point of the
		range, if the value is greater than all the values in the
		range, or 3) the index of the value greater than the target
		value, if the value lies between two of (or less than all
		of) the values in the range.
 }
function CFArrayBSearchValues(theArray: CFArrayRef; range: CFRange;
  value: Pointer; comparator: CFComparatorFunction; context: Pointer): CFIndex; cdecl;
  external CoreFoundationLib name _PU + 'CFArrayBSearchValues';
{$EXTERNALSYM CFArrayBSearchValues}

{ !
	@function CFArrayAppendValue
	Adds the value to the array giving it a new largest index.
	@param theArray The array to which the value is to be added. If this
		parameter is not a valid mutable CFArray, the behavior is
		undefined.
	@param value The value to add to the array. The value is retained by
		the array using the retain callback provided when the array
		was created. If the value is not of the sort expected by the
		retain callback, the behavior is undefined. The value is
		assigned to the index one larger than the previous largest
		index, and the count of the array is increased by one.
 }
procedure CFArrayAppendValue(theArray: CFMutableArrayRef; value: Pointer); cdecl;
  external CoreFoundationLib name _PU + 'CFArrayAppendValue';
{$EXTERNALSYM CFArrayAppendValue}

{ !
	@function CFArrayInsertValueAtIndex
	Adds the value to the array, giving it the given index.
	@param theArray The array to which the value is to be added. If this
		parameter is not a valid mutable CFArray, the behavior is
		undefined.
	@param idx The index to which to add the new value. If the index is
		outside the index space of the array (0 to N inclusive,
		where N is the count of the array before the operation), the
		behavior is undefined. If the index is the same as N, this
		function has the same effect as CFArrayAppendValue().
	@param value The value to add to the array. The value is retained by
		the array using the retain callback provided when the array
		was created. If the value is not of the sort expected by the
		retain callback, the behavior is undefined. The value is
		assigned to the given index, and all values with equal and
		larger indices have their indexes increased by one.
 }
procedure CFArrayInsertValueAtIndex(theArray: CFMutableArrayRef; idx: CFIndex;
  value: Pointer); cdecl;
  external CoreFoundationLib name _PU + 'CFArrayInsertValueAtIndex';
{$EXTERNALSYM CFArrayInsertValueAtIndex}

{ !
	@function CFArraySetValueAtIndex
	Changes the value with the given index in the array.
	@param theArray The array in which the value is to be changed. If this
		parameter is not a valid mutable CFArray, the behavior is
		undefined.
	@param idx The index to which to set the new value. If the index is
		outside the index space of the array (0 to N inclusive,
		where N is the count of the array before the operation), the
		behavior is undefined. If the index is the same as N, this
		function has the same effect as CFArrayAppendValue().
	@param value The value to set in the array. The value is retained by
		the array using the retain callback provided when the array
		was created, and the previous value with that index is
		released. If the value is not of the sort expected by the
		retain callback, the behavior is undefined. The indices of
		other values is not affected.
 }
procedure CFArraySetValueAtIndex(theArray: CFMutableArrayRef; idx: CFIndex;
  value: Pointer); cdecl;
  external CoreFoundationLib name _PU + 'CFArraySetValueAtIndex';
{$EXTERNALSYM CFArraySetValueAtIndex}

{ !
	@function CFArrayRemoveValueAtIndex
	Removes the value with the given index from the array.
	@param theArray The array from which the value is to be removed. If
		this parameter is not a valid mutable CFArray, the behavior
		is undefined.
	@param idx The index from which to remove the value. If the index is
		outside the index space of the array (0 to N-1 inclusive,
		where N is the count of the array before the operation), the
		behavior is undefined.
 }
procedure CFArrayRemoveValueAtIndex(theArray: CFMutableArrayRef; idx: CFIndex); cdecl;
  external CoreFoundationLib name _PU + 'CFArrayRemoveValueAtIndex';
{$EXTERNALSYM CFArrayRemoveValueAtIndex}

{ !
	@function CFArrayRemoveAllValues
	Removes all the values from the array, making it empty.
	@param theArray The array from which all of the values are to be
		removed. If this parameter is not a valid mutable CFArray,
		the behavior is undefined.
 }
procedure CFArrayRemoveAllValues(theArray: CFMutableArrayRef); cdecl;
  external CoreFoundationLib name _PU + 'CFArrayRemoveAllValues';
{$EXTERNALSYM CFArrayRemoveAllValues}

{ !
	@function CFArrayReplaceValues
	Replaces a range of values in the array.
	@param theArray The array from which all of the values are to be
		removed. If this parameter is not a valid mutable CFArray,
		the behavior is undefined.
	@param range The range of values within the array to replace. If the
		range location or end point (defined by the location plus
		length minus 1) is outside the index space of the array (0
		to N inclusive, where N is the count of the array), the
		behavior is undefined. If the range length is negative, the
		behavior is undefined. The range may be empty (length 0),
		in which case the new values are merely inserted at the
		range location.
	@param newValues A C array of the pointer-sized values to be placed
		into the array. The new values in the array are ordered in
		the same order in which they appear in this C array. This
		parameter may be NULL if the newCount parameter is 0. This
		C array is not changed or freed by this function. If this
		parameter is not a valid pointer to a C array of at least
		newCount pointers, the behavior is undefined.
	@param newCount The number of values to copy from the values C
		array into the CFArray. If this parameter is different than
		the range length, the excess newCount values will be
		inserted after the range, or the excess range values will be
		deleted. This parameter may be 0, in which case no new
		values are replaced into the array and the values in the
		range are simply removed. If this parameter is negative, or
		greater than the number of values actually in the newValues
		C array, the behavior is undefined.
 }
procedure CFArrayReplaceValues(theArray: CFMutableArrayRef; range: CFRange;
  newValues: Pointer; newCount: CFIndex); cdecl;
  external CoreFoundationLib name _PU + 'CFArrayReplaceValues';
{$EXTERNALSYM CFArrayReplaceValues}

{ !
	@function CFArrayExchangeValuesAtIndices
	Exchanges the values at two indices of the array.
	@param theArray The array of which the values are to be swapped. If
		this parameter is not a valid mutable CFArray, the behavior
		is undefined.
	@param idx1 The first index whose values should be swapped. If the
		index is outside the index space of the array (0 to N-1
		inclusive, where N is the count of the array before the
		operation), the behavior is undefined.
	@param idx2 The second index whose values should be swapped. If the
		index is outside the index space of the array (0 to N-1
		inclusive, where N is the count of the array before the
		operation), the behavior is undefined.
 }
procedure CFArrayExchangeValuesAtIndices(theArray: CFMutableArrayRef;
  idx1: CFIndex; idx2: CFIndex); cdecl;
  external CoreFoundationLib name _PU + 'CFArrayExchangeValuesAtIndices';
{$EXTERNALSYM CFArrayExchangeValuesAtIndices}

{ !
	@function CFArraySortValues
	Sorts the values in the array using the given comparison function.
	@param theArray The array whose values are to be sorted. If this
		parameter is not a valid mutable CFArray, the behavior is
		undefined.
	@param range The range of values within the array to sort. If the
		range location or end point (defined by the location plus
		length minus 1) is outside the index space of the array (0
		to N-1 inclusive, where N is the count of the array), the
		behavior is undefined. If the range length is negative, the
		behavior is undefined. The range may be empty (length 0).
	@param comparator The function with the comparator function type
		signature which is used in the sort operation to compare
		values in the array with the given value. If this parameter
		is not a pointer to a function of the correct prototype, the
		the behavior is undefined. If there are values in the array
		which the comparator function does not expect or cannot
		properly compare, the behavior is undefined. The values in
		the range are sorted from least to greatest according to
		this function.
	@param context A pointer-sized user-defined value, which is passed
		as the third parameter to the comparator function, but is
		otherwise unused by this function. If the context is not
		what is expected by the comparator function, the behavior is
		undefined.
 }
procedure CFArraySortValues(theArray: CFMutableArrayRef; range: CFRange;
  comparator: CFComparatorFunction; context: Pointer); cdecl;
  external CoreFoundationLib name _PU + 'CFArraySortValues';
{$EXTERNALSYM CFArraySortValues}

{ !
	@function CFArrayAppendArray
	Adds the values from an array to another array.
	@param theArray The array to which values from the otherArray are to
		be added. If this parameter is not a valid mutable CFArray,
		the behavior is undefined.
	@param otherArray The array providing the values to be added to the
		array. If this parameter is not a valid CFArray, the
		behavior is undefined.
	@param otherRange The range within the otherArray from which to add
		the values to the array. If the range location or end point
		(defined by the location plus length minus 1) is outside
		the index space of the otherArray (0 to N-1 inclusive, where
		N is the count of the otherArray), the behavior is
		undefined. The new values are retained by the array using
		the retain callback provided when the array was created. If
		the values are not of the sort expected by the retain
		callback, the behavior is undefined. The values are assigned
		to the indices one larger than the previous largest index
		in the array, and beyond, and the count of the array is
		increased by range.length. The values are assigned new
		indices in the array from smallest to largest index in the
		order in which they appear in the otherArray.
 }
procedure CFArrayAppendArray(theArray: CFMutableArrayRef;
  otherArray: CFArrayRef; otherRange: CFRange); cdecl;
  external CoreFoundationLib name _PU + 'CFArrayAppendArray';
{$EXTERNALSYM CFArrayAppendArray}

{$ENDIF !__COREFOUNDATION_CFARRAY__}

