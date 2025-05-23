{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2010-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{ From dispatch/semaphore.h }
type
  dispatch_object = record end;
  dispatch_object_t = ^dispatch_object;
  dispatch_semaphore_t = dispatch_object_t;
  dispatch_time_t = UInt64;

const
  DISPATCH_TIME_FOREVER:UInt64 = $ffffffffffffffff;
  {$EXTERNALSYM DISPATCH_TIME_FOREVER}
  DISPATCH_TIME_NOW:UInt64 = 0;
  {$EXTERNALSYM DISPATCH_TIME_NOW}
  NSEC_PER_MSEC:UInt64 = 1000000;
  {$EXTERNALSYM NSEC_PER_MSEC}

function dispatch_semaphore_create(value: LongInt): dispatch_semaphore_t; cdecl;
  {$EXTERNALSYM dispatch_semaphore_create}

function dispatch_semaphore_wait(dsema: dispatch_semaphore_t; timeout: dispatch_time_t): LongInt; cdecl;
  {$EXTERNALSYM dispatch_semaphore_wait}

function dispatch_semaphore_signal(dsema: dispatch_semaphore_t): LongInt; cdecl;
  {$EXTERNALSYM dispatch_semaphore_signal}

procedure dispatch_release(obj: dispatch_object_t); cdecl;
  {$EXTERNALSYM dispatch_release}

function dispatch_time(when: dispatch_time_t; delta: Int64): dispatch_time_t; cdecl;
  {$EXTERNALSYM dispatch_time}

