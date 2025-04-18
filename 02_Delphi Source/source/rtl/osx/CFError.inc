{*******************************************************}
{                                                       }
{              Delphi Runtime Library                   }
{                                                       }
{          File: CFError.h                              }
{          Copyright (c) 2006-2009, Apple Inc.          }
{          All Rights Reserved.                         }
{                                                       }
{       Translator: Embarcadero Technologies, Inc.      }
{ Copyright(c) 2010-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

(* !
	@header CFError
        @discussion
            CFErrors are used to encompass information about errors. At minimum,
            errors are identified by their domain (a string) and an error code
            within that domain. In addition a "userInfo" dictionary supplied at
            creation time enables providing additional info that might be useful
            for the interpretation and reporting of the error. This dictionary
            can even contain an "underlying" error, which is wrapped as an error
            bubbles up through various layers.
            
            CFErrors have the ability to provide human-readable descriptions
            for the errors; in fact, they are designed to provide localizable,
            end-user presentable errors that can appear in the UI. CFError has
            a number of predefined userInfo keys to enable developers to supply
            the info.

            Usage recommendation for CFErrors is to return them as by-ref
            parameters in functions. This enables the caller to pass NULL in
            when they don't actually want information about the error. The
            presence of an error should be reported by other means, for
            instance a NULL or false return value from the function
            call proper:

            CFError *error;
            if (!ReadFromFile(fd, &error)) {
                ... process error ...
                CFRelease(error);   // If an error occurs, the returned CFError must be released.
            }

            It is the responsibility of anyone returning CFErrors this way to:
            - Not touch the error argument if no error occurs
            - Create and assign the error for return only if the error argument is non-NULL

            In addition, it's recommended that CFErrors be used in error
            situations only (not status), and where there are multiple
            possible errors to distinguish between. For instance there is
            no plan to add CFErrors to existing APIs in CF which currently
            don't return errors; in many cases, there is one possible reason
            for failure, and a false or NULL return is enough to indicate it.

            CFError is toll-free bridged to NSError in Foundation. NSError
            in Foundation has some additional guidelines which makes it easy
            to automatically report errors to users and even try to recover
            from them.
            See http://developer.apple.com/documentation/Cocoa/Conceptual/ErrorHandlingCocoa/ErrorHandling/chapter_1_section_1.html
            for more info on NSError programming guidelines.
 *)

{$IFNDEF __COREFOUNDATION_CFERROR__}

{$DEFINE __COREFOUNDATION_CFERROR__}

{$I CFBase.inc}
{$I CFDictionary.inc}
{$I CFString.inc}

{ !
	@typedef CFErrorRef
	    This is the type of a reference to CFErrors.  CFErrorRef is toll-free bridged with NSError.
 }
type
  PCFErrorRef = ^CFErrorRef;
  CFErrorRef = ^__CFError;
  {$EXTERNALSYM CFErrorRef}
  __CFError = record
  end;
  {$EXTERNALSYM __CFError}

{ !
	@function CFErrorGetTypeID
	    Returns the type identifier of all CFError instances.
 }
function CFErrorGetTypeID: CFTypeID; cdecl;
  external CoreFoundationLib name _PU + 'CFErrorGetTypeID';
{$EXTERNALSYM CFErrorGetTypeID}


// Predefined domains; value of "code" will correspond to preexisting values in these domains.
function kCFErrorDomainPOSIX: CFStringRef;
{$EXTERNALSYM kCFErrorDomainPOSIX}
function kCFErrorDomainOSStatus: CFStringRef;
{$EXTERNALSYM kCFErrorDomainOSStatus}
function kCFErrorDomainMach: CFStringRef;
{$EXTERNALSYM kCFErrorDomainMach}
function kCFErrorDomainCocoa: CFStringRef;
{$EXTERNALSYM kCFErrorDomainCocoa}

// Keys in userInfo for localizable, end-user presentable error messages. At minimum provide one of first two; ideally provide all three.
function kCFErrorLocalizedDescriptionKey: CFStringRef;
{$EXTERNALSYM kCFErrorLocalizedDescriptionKey}
function kCFErrorLocalizedFailureReasonKey: CFStringRef;
{$EXTERNALSYM kCFErrorLocalizedFailureReasonKey}
function kCFErrorLocalizedRecoverySuggestionKey: CFStringRef;
{$EXTERNALSYM kCFErrorLocalizedRecoverySuggestionKey}

// If you do not have localizable error strings, you can provide a value for this key instead.
function kCFErrorDescriptionKey: CFStringRef;
{$EXTERNALSYM kCFErrorDescriptionKey}

// Other keys in userInfo.
function kCFErrorUnderlyingErrorKey: CFStringRef;
{$EXTERNALSYM kCFErrorUnderlyingErrorKey}


{ !
	@function CFErrorCreate
	@abstract Creates a new CFError.
	@param allocator The CFAllocator which should be used to allocate memory for the error. This parameter may be NULL in which case the 
	    current default CFAllocator is used. If this reference is not a valid CFAllocator, the behavior is undefined.
	@param domain A CFString identifying the error domain. If this reference is NULL or is otherwise not a valid CFString, the behavior is undefined.
	@param code A CFIndex identifying the error code. The code is interpreted within the context of the error domain.
	@param userInfo A CFDictionary created with kCFCopyStringDictionaryKeyCallBacks and kCFTypeDictionaryValueCallBacks. It will be copied with CFDictionaryCreateCopy(). 
	    If no userInfo dictionary is desired, NULL may be passed in as a convenience, in which case an empty userInfo dictionary will be assigned.
	@result A reference to the new CFError.
 }
function CFErrorCreate(allocator: CFAllocatorRef; domain: CFStringRef; 
  code: CFIndex; userInfo: CFDictionaryRef): CFErrorRef; cdecl;
  external CoreFoundationLib name _PU + 'CFErrorCreate';
{$EXTERNALSYM CFErrorCreate}

{ !
	@function CFErrorCreateWithUserInfoKeysAndValues
	@abstract Creates a new CFError without having to create an intermediate userInfo dictionary.
	@param allocator The CFAllocator which should be used to allocate memory for the error. This parameter may be NULL in which case the 
	    current default CFAllocator is used. If this reference is not a valid CFAllocator, the behavior is undefined.
	@param domain A CFString identifying the error domain. If this reference is NULL or is otherwise not a valid CFString, the behavior is undefined.
	@param code A CFIndex identifying the error code. The code is interpreted within the context of the error domain.
	@param userInfoKeys An array of numUserInfoValues CFStrings used as keys in creating the userInfo dictionary. NULL is valid only if numUserInfoValues is 0.
	@param userInfoValues An array of numUserInfoValues CF types used as values in creating the userInfo dictionary.  NULL is valid only if numUserInfoValues is 0.
	@param numUserInfoValues CFIndex representing the number of keys and values in the userInfoKeys and userInfoValues arrays.
	@result A reference to the new CFError. numUserInfoValues CF types are gathered from each of userInfoKeys and userInfoValues to create the userInfo dictionary.
 }
function CFErrorCreateWithUserInfoKeysAndValues(allocator: CFAllocatorRef; 
  domain: CFStringRef; code: CFIndex; userInfoKeys: Pointer;
  userInfoValues: Pointer; numUserInfoValues: CFIndex): CFErrorRef; cdecl;
  external CoreFoundationLib name _PU + 'CFErrorCreateWithUserInfoKeysAndValues';
{$EXTERNALSYM CFErrorCreateWithUserInfoKeysAndValues}

{ !
	@function CFErrorGetDomain
	@abstract Returns the error domain the CFError was created with.
	@param err The CFError whose error domain is to be returned. If this reference is not a valid CFError, the behavior is undefined.
	@result The error domain of the CFError. Since this is a "Get" function, the caller shouldn't CFRelease the return value.
 }
function CFErrorGetDomain(err: CFErrorRef): CFStringRef; cdecl;
  external CoreFoundationLib name _PU + 'CFErrorGetDomain';
{$EXTERNALSYM CFErrorGetDomain}

{ !
	@function CFErrorGetCode
	@abstract Returns the error code the CFError was created with.
	@param err The CFError whose error code is to be returned. If this reference is not a valid CFError, the behavior is undefined.
	@result The error code of the CFError (not an error return for the current call).
 }
function CFErrorGetCode(err: CFErrorRef): CFIndex; cdecl;
  external CoreFoundationLib name _PU + 'CFErrorGetCode';
{$EXTERNALSYM CFErrorGetCode}

{ !
	@function CFErrorCopyUserInfo
        @abstract Returns CFError userInfo dictionary.
	@discussion Returns a dictionary containing the same keys and values as in the userInfo dictionary the CFError was created with. Returns an empty dictionary if NULL was supplied to CFErrorCreate().
	@param err The CFError whose error user info is to be returned. If this reference is not a valid CFError, the behavior is undefined.
	@result The user info of the CFError.
 }
function CFErrorCopyUserInfo(err: CFErrorRef): CFDictionaryRef; cdecl;
  external CoreFoundationLib name _PU + 'CFErrorCopyUserInfo';
{$EXTERNALSYM CFErrorCopyUserInfo}

{ !
	@function CFErrorCopyDescription
	@abstract Returns a human-presentable description for the error. CFError creators should strive to make sure the return value is human-presentable and localized by providing a value for kCFErrorLocalizedDescriptionKey at the time of CFError creation.
        @discussion This is a complete sentence or two which says what failed and why it failed. Rules for computing the return value:
            - Look for kCFErrorLocalizedDescriptionKey in the user info and if not NULL, returns that as-is.  
            - Otherwise, if there is a kCFErrorLocalizedFailureReasonKey in the user info, generate an error from that. Something like: "Operation code not be completed. " + kCFErrorLocalizedFailureReasonKey
            - Otherwise, generate a semi-user presentable string from kCFErrorDescriptionKey, the domain, and code. Something like: "Operation could not be completed. Error domain/code occurred. " or "Operation could not be completed. " + kCFErrorDescriptionKey + " (Error domain/code)"
            Toll-free bridged NSError instances might provide additional behaviors for manufacturing a description string.  Do not count on the exact contents or format of the returned string, it might change.
	@param err The CFError whose description is to be returned. If this reference is not a valid CFError, the behavior is undefined.
	@result A CFString with human-presentable description of the CFError. Never NULL.
 }
function CFErrorCopyDescription(err: CFErrorRef): CFStringRef; cdecl;
  external CoreFoundationLib name _PU + 'CFErrorCopyDescription';
{$EXTERNALSYM CFErrorCopyDescription}

{ !
	@function CFErrorCopyFailureReason
        @abstract Returns a human-presentable failure reason for the error.  May return NULL.  CFError creators should strive to make sure the return value is human-presentable and localized by providing a value for kCFErrorLocalizedFailureReasonKey at the time of CFError creation.
        @discussion This is a complete sentence which describes why the operation failed. In many cases this will be just the "because" part of the description (but as a complete sentence, which makes localization easier). By default this looks for kCFErrorLocalizedFailureReasonKey in the user info. Toll-free bridged NSError instances might provide additional behaviors for manufacturing this value. If no user-presentable string is available, returns NULL.
            Example Description: "Could not save file 'Letter' in folder 'Documents' because the volume 'MyDisk' doesn't have enough space."
            Corresponding FailureReason: "The volume 'MyDisk' doesn't have enough space."
	@param err The CFError whose failure reason is to be returned. If this reference is not a valid CFError, the behavior is undefined.
	@result A CFString with the localized, end-user presentable failure reason of the CFError, or NULL. 
 }
function CFErrorCopyFailureReason(err: CFErrorRef): CFStringRef; cdecl;
  external CoreFoundationLib name _PU + 'CFErrorCopyFailureReason';
{$EXTERNALSYM CFErrorCopyFailureReason}

{ !
	@function CFErrorCopyRecoverySuggestion
        @abstract Returns a human presentable recovery suggestion for the error.  May return NULL.  CFError creators should strive to make sure the return value is human-presentable and localized by providing a value for kCFErrorLocalizedRecoverySuggestionKey at the time of CFError creation.
        @discussion This is the string that can be displayed as the "informative" (aka "secondary") message on an alert panel. By default this looks for kCFErrorLocalizedRecoverySuggestionKey in the user info. Toll-free bridged NSError instances might provide additional behaviors for manufacturing this value. If no user-presentable string is available, returns NULL.
            Example Description: "Could not save file 'Letter' in folder 'Documents' because the volume 'MyDisk' doesn't have enough space."
            Corresponding RecoverySuggestion: "Remove some files from the volume and try again."
	@param err The CFError whose recovery suggestion is to be returned. If this reference is not a valid CFError, the behavior is undefined.
	@result A CFString with the localized, end-user presentable recovery suggestion of the CFError, or NULL. 
 }
function CFErrorCopyRecoverySuggestion(err: CFErrorRef): CFStringRef; cdecl;
  external CoreFoundationLib name _PU + 'CFErrorCopyRecoverySuggestion';
{$EXTERNALSYM CFErrorCopyRecoverySuggestion}


{$ENDIF __COREFOUNDATION_CFERROR__}

