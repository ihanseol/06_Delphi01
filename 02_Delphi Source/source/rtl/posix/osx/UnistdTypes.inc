{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}


const
{$IF declared(PosixStdio)}
  SEEK_SET = PosixStdio.SEEK_SET;             { set file offset to offset }
  {$EXTERNALSYM SEEK_SET}
  SEEK_CUR = PosixStdio.SEEK_CUR;             { set file offset to current plus offset }
  {$EXTERNALSYM SEEK_CUR}
  SEEK_END = PosixStdio.SEEK_END;             { set file offset to EOF plus offset }
  {$EXTERNALSYM SEEK_END}
{$ENDIF}

  STDIN_FILENO         = 0;
  {$EXTERNALSYM STDIN_FILENO}
  STDOUT_FILENO        = 1;
  {$EXTERNALSYM STDOUT_FILENO}
  STDERR_FILENO        = 2;
  {$EXTERNALSYM STDERR_FILENO}

  F_OK = 0;
  {$EXTERNALSYM F_OK}
  X_OK = 1 shl 0;
  {$EXTERNALSYM X_OK}
  W_OK = 1 shl 1;
  {$EXTERNALSYM W_OK}
  R_OK = 1 shl 2;
  {$EXTERNALSYM R_OK}

{
 * Extended access functions.
 * Note that we depend on these matching the definitions in sys/kauth.h,
 * but with the bits shifted left by 8.
 }
  _READ_OK = 1 shl 9 platform;       { read file data / read directory }
  {$EXTERNALSYM _READ_OK}
  _WRITE_OK = 1 shl 10 platform;     { write file data / add file to directory }
  {$EXTERNALSYM _WRITE_OK}
  _EXECUTE_OK = 1 shl 11 platform;   { execute file / search in directory }
  {$EXTERNALSYM _EXECUTE_OK}
  _DELETE_OK = 1 shl 12 platform;    { delete file / delete directory }
  {$EXTERNALSYM _DELETE_OK}
  _APPEND_OK = 1 shl 13 platform;    { append to file / add subdirectory to directory }
  {$EXTERNALSYM _APPEND_OK}
  _RMFILE_OK = 1 shl 14 platform;    { - / remove file from directory }
  {$EXTERNALSYM _RMFILE_OK}
  _RATTR_OK = 1 shl 15 platform;     { read basic attributes }
  {$EXTERNALSYM _RATTR_OK}
  _WATTR_OK = 1 shl 16 platform;     { write basic attributes }
  {$EXTERNALSYM _WATTR_OK}
  _REXT_OK = 1 shl 17 platform;      { read extended attributes }
  {$EXTERNALSYM _REXT_OK}
  _WEXT_OK = 1 shl 18 platform;      { write extended attributes }
  {$EXTERNALSYM _WEXT_OK}
  _RPERM_OK = 1 shl 19 platform;     { read permissions }
  {$EXTERNALSYM _RPERM_OK}
  _WPERM_OK = 1 shl 20 platform;     { write permissions }
  {$EXTERNALSYM _WPERM_OK}
  _CHOWN_OK = 1 shl 21 platform;     { change ownership }
  {$EXTERNALSYM _CHOWN_OK}

  _ACCESS_EXTENDED_MASK = _READ_OK or _WRITE_OK or _EXECUTE_OK or
    _DELETE_OK or _APPEND_OK or
    _RMFILE_OK or _REXT_OK or
    _WEXT_OK or _RATTR_OK or _WATTR_OK or _RPERM_OK or
    _WPERM_OK or _CHOWN_OK platform;
  {$EXTERNALSYM _ACCESS_EXTENDED_MASK}

{ Version test macros }
  _POSIX_VERSION = 200112;
  {$EXTERNALSYM _POSIX_VERSION}
  _POSIX2_VERSION = 200112;
  {$EXTERNALSYM _POSIX2_VERSION}
  _POSIX_VDISABLE = Byte($FF);
  {$EXTERNALSYM _POSIX_VDISABLE}
  _POSIX_THREAD_KEYS_MAX = 128 platform;
  {$EXTERNALSYM _POSIX_THREAD_KEYS_MAX}
{ _POSIX_VERSION and _POSIX2_VERSION from sys/unistd.h }
  _XOPEN_VERSION = 600;                             { [XSI] }
  {$EXTERNALSYM _XOPEN_VERSION}
  _XOPEN_XCU_VERSION = 4;                           { Older standard }
  {$EXTERNALSYM _XOPEN_XCU_VERSION}


{ Please keep this list in the same order as the applicable standard }
  _POSIX_ADVISORY_INFO = -1;                        { [ADV] }
  {$EXTERNALSYM _POSIX_ADVISORY_INFO}
  _POSIX_ASYNCHRONOUS_IO = -1;                      { [AIO] }
  {$EXTERNALSYM _POSIX_ASYNCHRONOUS_IO}
  _POSIX_BARRIERS = -1;                             { [BAR] }
  {$EXTERNALSYM _POSIX_BARRIERS}
  _POSIX_CHOWN_RESTRICTED = 200112;
  {$EXTERNALSYM _POSIX_CHOWN_RESTRICTED}
  _POSIX_CLOCK_SELECTION = -1;                      { [CS] }
  {$EXTERNALSYM _POSIX_CLOCK_SELECTION}
  _POSIX_CPUTIME = -1;                              { [CPT] }
  {$EXTERNALSYM _POSIX_CPUTIME}
  _POSIX_FSYNC = 200112;                            { [FSC] }
  {$EXTERNALSYM _POSIX_FSYNC}
  _POSIX_IPV6 = 200112;
  {$EXTERNALSYM _POSIX_IPV6}
  _POSIX_JOB_CONTROL = 200112;
  {$EXTERNALSYM _POSIX_JOB_CONTROL}
  _POSIX_MAPPED_FILES = 200112;                     { [MF] }
  {$EXTERNALSYM _POSIX_MAPPED_FILES}
  _POSIX_MEMLOCK = -1;                              { [ML] }
  {$EXTERNALSYM _POSIX_MEMLOCK}
  _POSIX_MEMLOCK_RANGE = -1;                        { [MR] }
  {$EXTERNALSYM _POSIX_MEMLOCK_RANGE}
  _POSIX_MEMORY_PROTECTION = 200112;                { [MPR] }
  {$EXTERNALSYM _POSIX_MEMORY_PROTECTION}
  _POSIX_MESSAGE_PASSING = -1;                      { [MSG] }
  {$EXTERNALSYM _POSIX_MESSAGE_PASSING}
  _POSIX_MONOTONIC_CLOCK = -1;                      { [MON] }
  {$EXTERNALSYM _POSIX_MONOTONIC_CLOCK}
  _POSIX_NO_TRUNC = 200112;
  {$EXTERNALSYM _POSIX_NO_TRUNC}
  _POSIX_PRIORITIZED_IO = -1;                       { [PIO] }
  {$EXTERNALSYM _POSIX_PRIORITIZED_IO}
  _POSIX_PRIORITY_SCHEDULING = -1;                  { [PS] }
  {$EXTERNALSYM _POSIX_PRIORITY_SCHEDULING}
  _POSIX_RAW_SOCKETS = -1;                          { [RS] }
  {$EXTERNALSYM _POSIX_RAW_SOCKETS}
  _POSIX_READER_WRITER_LOCKS = 200112;              { [THR] }
  {$EXTERNALSYM _POSIX_READER_WRITER_LOCKS}
  _POSIX_REALTIME_SIGNALS = -1;                     { [RTS] }
  {$EXTERNALSYM _POSIX_REALTIME_SIGNALS}
  _POSIX_REGEXP = 200112;
  {$EXTERNALSYM _POSIX_REGEXP}
  _POSIX_SAVED_IDS = 200112;                        { XXX required }
  {$EXTERNALSYM _POSIX_SAVED_IDS}
  _POSIX_SEMAPHORES = -1;                           { [SEM] }
  {$EXTERNALSYM _POSIX_SEMAPHORES}
  _POSIX_SHARED_MEMORY_OBJECTS = -1;                { [SHM] }
  {$EXTERNALSYM _POSIX_SHARED_MEMORY_OBJECTS}
  _POSIX_SHELL = 200112;
  {$EXTERNALSYM _POSIX_SHELL}
  _POSIX_SPAWN = -1;                                { [SPN] }
  {$EXTERNALSYM _POSIX_SPAWN}
  _POSIX_SPIN_LOCKS = -1;                           { [SPI] }
  {$EXTERNALSYM _POSIX_SPIN_LOCKS}
  _POSIX_SPORADIC_SERVER = -1;                      { [SS] }
  {$EXTERNALSYM _POSIX_SPORADIC_SERVER}
  _POSIX_SYNCHRONIZED_IO = -1;                      { [SIO] }
  {$EXTERNALSYM _POSIX_SYNCHRONIZED_IO}
  _POSIX_THREAD_ATTR_STACKADDR = 200112;            { [TSA] }
  {$EXTERNALSYM _POSIX_THREAD_ATTR_STACKADDR}
  _POSIX_THREAD_ATTR_STACKSIZE = 200112;            { [TSS] }
  {$EXTERNALSYM _POSIX_THREAD_ATTR_STACKSIZE}
  _POSIX_THREAD_CPUTIME = -1;                       { [TCT] }
  {$EXTERNALSYM _POSIX_THREAD_CPUTIME}
  _POSIX_THREAD_PRIO_INHERIT = -1;                  { [TPI] }
  {$EXTERNALSYM _POSIX_THREAD_PRIO_INHERIT}
  _POSIX_THREAD_PRIO_PROTECT = -1;                  { [TPP] }
  {$EXTERNALSYM _POSIX_THREAD_PRIO_PROTECT}
  _POSIX_THREAD_PRIORITY_SCHEDULING = -1;           { [TPS] }
  {$EXTERNALSYM _POSIX_THREAD_PRIORITY_SCHEDULING}
  _POSIX_THREAD_PROCESS_SHARED = 200112;            { [TSH] }
  {$EXTERNALSYM _POSIX_THREAD_PROCESS_SHARED}
  _POSIX_THREAD_SAFE_FUNCTIONS = 200112;            { [TSF] }
  {$EXTERNALSYM _POSIX_THREAD_SAFE_FUNCTIONS}
  _POSIX_THREAD_SPORADIC_SERVER = -1;               { [TSP] }
  {$EXTERNALSYM _POSIX_THREAD_SPORADIC_SERVER}
  _POSIX_THREADS = 200112;                          { [THR] }
  {$EXTERNALSYM _POSIX_THREADS}
  _POSIX_TIMEOUTS = -1;                             { [TMO] }
  {$EXTERNALSYM _POSIX_TIMEOUTS}
  _POSIX_TIMERS = -1;                               { [TMR] }
  {$EXTERNALSYM _POSIX_TIMERS}
  _POSIX_TRACE = -1;                                { [TRC] }
  {$EXTERNALSYM _POSIX_TRACE}
  _POSIX_TRACE_EVENT_FILTER = -1;                   { [TEF] }
  {$EXTERNALSYM _POSIX_TRACE_EVENT_FILTER}
  _POSIX_TRACE_INHERIT = -1;                        { [TRI] }
  {$EXTERNALSYM _POSIX_TRACE_INHERIT}
  _POSIX_TRACE_LOG = -1;                            { [TRL] }
  {$EXTERNALSYM _POSIX_TRACE_LOG}
  _POSIX_TYPED_MEMORY_OBJECTS = -1;                 { [TYM] }
  {$EXTERNALSYM _POSIX_TYPED_MEMORY_OBJECTS}

  _POSIX2_C_BIND = 200112;
  {$EXTERNALSYM _POSIX2_C_BIND}
  _POSIX2_C_DEV = 200112;                           { c99 command }
  {$EXTERNALSYM _POSIX2_C_DEV}
  _POSIX2_CHAR_TERM = 200112;
  {$EXTERNALSYM _POSIX2_CHAR_TERM}
  _POSIX2_FORT_DEV = -1;                            { fort77 command }
  {$EXTERNALSYM _POSIX2_FORT_DEV}
  _POSIX2_FORT_RUN = 200112;
  {$EXTERNALSYM _POSIX2_FORT_RUN}
  _POSIX2_LOCALEDEF = 200112;                       { localedef command }
  {$EXTERNALSYM _POSIX2_LOCALEDEF}
  _POSIX2_PBS = -1;
  {$EXTERNALSYM _POSIX2_PBS}
  _POSIX2_PBS_ACCOUNTING = -1;
  {$EXTERNALSYM _POSIX2_PBS_ACCOUNTING}
  _POSIX2_PBS_CHECKPOINT = -1;
  {$EXTERNALSYM _POSIX2_PBS_CHECKPOINT}
  _POSIX2_PBS_LOCATE = -1;
  {$EXTERNALSYM _POSIX2_PBS_LOCATE}
  _POSIX2_PBS_MESSAGE = -1;
  {$EXTERNALSYM _POSIX2_PBS_MESSAGE}
  _POSIX2_PBS_TRACK = -1;
  {$EXTERNALSYM _POSIX2_PBS_TRACK}
  _POSIX2_SW_DEV = 200112;
  {$EXTERNALSYM _POSIX2_SW_DEV}
  _POSIX2_UPE = 200112;                     { XXXX no fc, newgrp, tabs }
  {$EXTERNALSYM _POSIX2_UPE}

  _V6_ILP32_OFF32 = -1 platform;
  {$EXTERNALSYM _V6_ILP32_OFF32}
  _V6_ILP32_OFFBIG = (1) platform;
  {$EXTERNALSYM _V6_ILP32_OFFBIG}
  _V6_LP64_OFF64 = -1 platform;
  {$EXTERNALSYM _V6_LP64_OFF64}
  _V6_LPBIG_OFFBIG = -1 platform;
  {$EXTERNALSYM _V6_LPBIG_OFFBIG}

  _POSIX_V6_ILP32_OFF32 = -1;
  {$EXTERNALSYM _POSIX_V6_ILP32_OFF32}
  _POSIX_V6_ILP32_OFFBIG = (1);
  {$EXTERNALSYM _POSIX_V6_ILP32_OFFBIG}
  _POSIX_V6_LP64_OFF64 = -1;
  {$EXTERNALSYM _POSIX_V6_LP64_OFF64}
  _POSIX_V6_LPBIG_OFFBIG = -1;
  {$EXTERNALSYM _POSIX_V6_LPBIG_OFFBIG}


  _XBS5_ILP32_OFF32 = _POSIX_V6_ILP32_OFF32;                      { legacy }
  {$EXTERNALSYM _XBS5_ILP32_OFF32}
  _XBS5_ILP32_OFFBIG = _POSIX_V6_ILP32_OFFBIG;                    { legacy }
  {$EXTERNALSYM _XBS5_ILP32_OFFBIG}
  _XBS5_LP64_OFF64 = _POSIX_V6_LP64_OFF64;                        { legacy }
  {$EXTERNALSYM _XBS5_LP64_OFF64}
  _XBS5_LPBIG_OFFBIG = _POSIX_V6_LPBIG_OFFBIG;                    { legacy }
  {$EXTERNALSYM _XBS5_LPBIG_OFFBIG}

  _XOPEN_CRYPT = (1);
  {$EXTERNALSYM _XOPEN_CRYPT}
  _XOPEN_ENH_I18N = (1);                            { XXX required }
  {$EXTERNALSYM _XOPEN_ENH_I18N}
  _XOPEN_LEGACY = -1;                       { no ftime gcvt, wcswcs }
  {$EXTERNALSYM _XOPEN_LEGACY}
  _XOPEN_REALTIME = -1;                     { no q'ed signals, mq_* }
  {$EXTERNALSYM _XOPEN_REALTIME}
  _XOPEN_REALTIME_THREADS = -1;             { no posix_spawn, et. al. }
  {$EXTERNALSYM _XOPEN_REALTIME_THREADS}
  _XOPEN_SHM = (1);
  {$EXTERNALSYM _XOPEN_SHM}
  _XOPEN_STREAMS = -1;
  {$EXTERNALSYM _XOPEN_STREAMS}
  _XOPEN_UNIX = (1);
  {$EXTERNALSYM _XOPEN_UNIX}


  F_ULOCK = 0;             { unlock locked section }
  {$EXTERNALSYM F_ULOCK}
  F_LOCK = 1;              { lock a section for exclusive use }
  {$EXTERNALSYM F_LOCK}
  F_TLOCK = 2;             { test and lock a section for exclusive use }
  {$EXTERNALSYM F_TLOCK}
  F_TEST = 3;              { test a section for locks by other procs }
  {$EXTERNALSYM F_TEST}

  _CS_PATH = 1;
  {$EXTERNALSYM _CS_PATH}
  _CS_POSIX_V6_ILP32_OFF32_CFLAGS = 2;
  {$EXTERNALSYM _CS_POSIX_V6_ILP32_OFF32_CFLAGS}
  _CS_POSIX_V6_ILP32_OFF32_LDFLAGS = 3;
  {$EXTERNALSYM _CS_POSIX_V6_ILP32_OFF32_LDFLAGS}
  _CS_POSIX_V6_ILP32_OFF32_LIBS = 4;
  {$EXTERNALSYM _CS_POSIX_V6_ILP32_OFF32_LIBS}
  _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS = 5;
  {$EXTERNALSYM _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS}
  _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS = 6;
  {$EXTERNALSYM _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS}
  _CS_POSIX_V6_ILP32_OFFBIG_LIBS = 7;
  {$EXTERNALSYM _CS_POSIX_V6_ILP32_OFFBIG_LIBS}
  _CS_POSIX_V6_LP64_OFF64_CFLAGS = 8;
  {$EXTERNALSYM _CS_POSIX_V6_LP64_OFF64_CFLAGS}
  _CS_POSIX_V6_LP64_OFF64_LDFLAGS = 9;
  {$EXTERNALSYM _CS_POSIX_V6_LP64_OFF64_LDFLAGS}
  _CS_POSIX_V6_LP64_OFF64_LIBS = 10;
  {$EXTERNALSYM _CS_POSIX_V6_LP64_OFF64_LIBS}
  _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS = 11;
  {$EXTERNALSYM _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS}
  _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS = 12;
  {$EXTERNALSYM _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS}
  _CS_POSIX_V6_LPBIG_OFFBIG_LIBS = 13;
  {$EXTERNALSYM _CS_POSIX_V6_LPBIG_OFFBIG_LIBS}
  _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS = 14;
  {$EXTERNALSYM _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS}

{ reserved for compatibility with Issue 5 }
  _CS_XBS5_ILP32_OFF32_CFLAGS = 20;
  {$EXTERNALSYM _CS_XBS5_ILP32_OFF32_CFLAGS}
  _CS_XBS5_ILP32_OFF32_LDFLAGS = 21;
  {$EXTERNALSYM _CS_XBS5_ILP32_OFF32_LDFLAGS}
  _CS_XBS5_ILP32_OFF32_LIBS = 22;
  {$EXTERNALSYM _CS_XBS5_ILP32_OFF32_LIBS}
  _CS_XBS5_ILP32_OFF32_LINTFLAGS = 23;
  {$EXTERNALSYM _CS_XBS5_ILP32_OFF32_LINTFLAGS}
  _CS_XBS5_ILP32_OFFBIG_CFLAGS = 24;
  {$EXTERNALSYM _CS_XBS5_ILP32_OFFBIG_CFLAGS}
  _CS_XBS5_ILP32_OFFBIG_LDFLAGS = 25;
  {$EXTERNALSYM _CS_XBS5_ILP32_OFFBIG_LDFLAGS}
  _CS_XBS5_ILP32_OFFBIG_LIBS = 26;
  {$EXTERNALSYM _CS_XBS5_ILP32_OFFBIG_LIBS}
  _CS_XBS5_ILP32_OFFBIG_LINTFLAGS = 27;
  {$EXTERNALSYM _CS_XBS5_ILP32_OFFBIG_LINTFLAGS}
  _CS_XBS5_LP64_OFF64_CFLAGS = 28;
  {$EXTERNALSYM _CS_XBS5_LP64_OFF64_CFLAGS}
  _CS_XBS5_LP64_OFF64_LDFLAGS = 29;
  {$EXTERNALSYM _CS_XBS5_LP64_OFF64_LDFLAGS}
  _CS_XBS5_LP64_OFF64_LIBS = 30;
  {$EXTERNALSYM _CS_XBS5_LP64_OFF64_LIBS}
  _CS_XBS5_LP64_OFF64_LINTFLAGS = 31;
  {$EXTERNALSYM _CS_XBS5_LP64_OFF64_LINTFLAGS}
  _CS_XBS5_LPBIG_OFFBIG_CFLAGS = 32;
  {$EXTERNALSYM _CS_XBS5_LPBIG_OFFBIG_CFLAGS}
  _CS_XBS5_LPBIG_OFFBIG_LDFLAGS = 33;
  {$EXTERNALSYM _CS_XBS5_LPBIG_OFFBIG_LDFLAGS}
  _CS_XBS5_LPBIG_OFFBIG_LIBS = 34;
  {$EXTERNALSYM _CS_XBS5_LPBIG_OFFBIG_LIBS}
  _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS = 35;
  {$EXTERNALSYM _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS}

  _CS_DARWIN_USER_DIR = 65536 platform;
  {$EXTERNALSYM _CS_DARWIN_USER_DIR}
  _CS_DARWIN_USER_TEMP_DIR = 65537 platform;
  {$EXTERNALSYM _CS_DARWIN_USER_TEMP_DIR}
  _CS_DARWIN_USER_CACHE_DIR = 65538 platform;
  {$EXTERNALSYM _CS_DARWIN_USER_CACHE_DIR}

{ configurable system variables }
  _SC_ARG_MAX = 1;
  {$EXTERNALSYM _SC_ARG_MAX}
  _SC_CHILD_MAX = 2;
  {$EXTERNALSYM _SC_CHILD_MAX}
  _SC_CLK_TCK = 3;
  {$EXTERNALSYM _SC_CLK_TCK}
  _SC_NGROUPS_MAX = 4;
  {$EXTERNALSYM _SC_NGROUPS_MAX}
  _SC_OPEN_MAX = 5;
  {$EXTERNALSYM _SC_OPEN_MAX}
  _SC_JOB_CONTROL = 6;
  {$EXTERNALSYM _SC_JOB_CONTROL}
  _SC_SAVED_IDS = 7;
  {$EXTERNALSYM _SC_SAVED_IDS}
  _SC_VERSION = 8;
  {$EXTERNALSYM _SC_VERSION}
  _SC_BC_BASE_MAX = 9;
  {$EXTERNALSYM _SC_BC_BASE_MAX}
  _SC_BC_DIM_MAX = 10;
  {$EXTERNALSYM _SC_BC_DIM_MAX}
  _SC_BC_SCALE_MAX = 11;
  {$EXTERNALSYM _SC_BC_SCALE_MAX}
  _SC_BC_STRING_MAX = 12;
  {$EXTERNALSYM _SC_BC_STRING_MAX}
  _SC_COLL_WEIGHTS_MAX = 13;
  {$EXTERNALSYM _SC_COLL_WEIGHTS_MAX}
  _SC_EXPR_NEST_MAX = 14;
  {$EXTERNALSYM _SC_EXPR_NEST_MAX}
  _SC_LINE_MAX = 15;
  {$EXTERNALSYM _SC_LINE_MAX}
  _SC_RE_DUP_MAX = 16;
  {$EXTERNALSYM _SC_RE_DUP_MAX}
  _SC_2_VERSION = 17;
  {$EXTERNALSYM _SC_2_VERSION}
  _SC_2_C_BIND = 18;
  {$EXTERNALSYM _SC_2_C_BIND}
  _SC_2_C_DEV = 19;
  {$EXTERNALSYM _SC_2_C_DEV}
  _SC_2_CHAR_TERM = 20;
  {$EXTERNALSYM _SC_2_CHAR_TERM}
  _SC_2_FORT_DEV = 21;
  {$EXTERNALSYM _SC_2_FORT_DEV}
  _SC_2_FORT_RUN = 22;
  {$EXTERNALSYM _SC_2_FORT_RUN}
  _SC_2_LOCALEDEF = 23;
  {$EXTERNALSYM _SC_2_LOCALEDEF}
  _SC_2_SW_DEV = 24;
  {$EXTERNALSYM _SC_2_SW_DEV}
  _SC_2_UPE = 25;
  {$EXTERNALSYM _SC_2_UPE}
  _SC_STREAM_MAX = 26;
  {$EXTERNALSYM _SC_STREAM_MAX}
  _SC_TZNAME_MAX = 27;
  {$EXTERNALSYM _SC_TZNAME_MAX}
  _SC_ASYNCHRONOUS_IO = 28;
  {$EXTERNALSYM _SC_ASYNCHRONOUS_IO}
  _SC_PAGESIZE = 29;
  {$EXTERNALSYM _SC_PAGESIZE}
  _SC_MEMLOCK = 30;
  {$EXTERNALSYM _SC_MEMLOCK}
  _SC_MEMLOCK_RANGE = 31;
  {$EXTERNALSYM _SC_MEMLOCK_RANGE}
  _SC_MEMORY_PROTECTION = 32;
  {$EXTERNALSYM _SC_MEMORY_PROTECTION}
  _SC_MESSAGE_PASSING = 33;
  {$EXTERNALSYM _SC_MESSAGE_PASSING}
  _SC_PRIORITIZED_IO = 34;
  {$EXTERNALSYM _SC_PRIORITIZED_IO}
  _SC_PRIORITY_SCHEDULING = 35;
  {$EXTERNALSYM _SC_PRIORITY_SCHEDULING}
  _SC_REALTIME_SIGNALS = 36;
  {$EXTERNALSYM _SC_REALTIME_SIGNALS}
  _SC_SEMAPHORES = 37;
  {$EXTERNALSYM _SC_SEMAPHORES}
  _SC_FSYNC = 38;
  {$EXTERNALSYM _SC_FSYNC}
  _SC_SHARED_MEMORY_OBJECTS = 39;
  {$EXTERNALSYM _SC_SHARED_MEMORY_OBJECTS}
  _SC_SYNCHRONIZED_IO = 40;
  {$EXTERNALSYM _SC_SYNCHRONIZED_IO}
  _SC_TIMERS = 41;
  {$EXTERNALSYM _SC_TIMERS}
  _SC_AIO_LISTIO_MAX = 42;
  {$EXTERNALSYM _SC_AIO_LISTIO_MAX}
  _SC_AIO_MAX = 43;
  {$EXTERNALSYM _SC_AIO_MAX}
  _SC_AIO_PRIO_DELTA_MAX = 44;
  {$EXTERNALSYM _SC_AIO_PRIO_DELTA_MAX}
  _SC_DELAYTIMER_MAX = 45;
  {$EXTERNALSYM _SC_DELAYTIMER_MAX}
  _SC_MQ_OPEN_MAX = 46;
  {$EXTERNALSYM _SC_MQ_OPEN_MAX}
  _SC_MAPPED_FILES = 47;                    { swap _SC_PAGESIZE vs. BSD }
  {$EXTERNALSYM _SC_MAPPED_FILES}
  _SC_RTSIG_MAX = 48;
  {$EXTERNALSYM _SC_RTSIG_MAX}
  _SC_SEM_NSEMS_MAX = 49;
  {$EXTERNALSYM _SC_SEM_NSEMS_MAX}
  _SC_SEM_VALUE_MAX = 50;
  {$EXTERNALSYM _SC_SEM_VALUE_MAX}
  _SC_SIGQUEUE_MAX = 51;
  {$EXTERNALSYM _SC_SIGQUEUE_MAX}
  _SC_TIMER_MAX = 52;
  {$EXTERNALSYM _SC_TIMER_MAX}
(* IF Expression: !defined(_POSIX_C_SOURCE) || defined(_DARWIN_C_SOURCE) *)
  _SC_NPROCESSORS_CONF = 57 platform;
  {$EXTERNALSYM _SC_NPROCESSORS_CONF}
  _SC_NPROCESSORS_ONLN = 58 platform;
  {$EXTERNALSYM _SC_NPROCESSORS_ONLN}
{/* !_POSIX_C_SOURCE || _DARWIN_C_SOURCE */}
  _SC_2_PBS = 59;
  {$EXTERNALSYM _SC_2_PBS}
  _SC_2_PBS_ACCOUNTING = 60;
  {$EXTERNALSYM _SC_2_PBS_ACCOUNTING}
  _SC_2_PBS_CHECKPOINT = 61;
  {$EXTERNALSYM _SC_2_PBS_CHECKPOINT}
  _SC_2_PBS_LOCATE = 62;
  {$EXTERNALSYM _SC_2_PBS_LOCATE}
  _SC_2_PBS_MESSAGE = 63;
  {$EXTERNALSYM _SC_2_PBS_MESSAGE}
  _SC_2_PBS_TRACK = 64;
  {$EXTERNALSYM _SC_2_PBS_TRACK}
  _SC_ADVISORY_INFO = 65;
  {$EXTERNALSYM _SC_ADVISORY_INFO}
  _SC_BARRIERS = 66;
  {$EXTERNALSYM _SC_BARRIERS}
  _SC_CLOCK_SELECTION = 67;
  {$EXTERNALSYM _SC_CLOCK_SELECTION}
  _SC_CPUTIME = 68;
  {$EXTERNALSYM _SC_CPUTIME}
  _SC_FILE_LOCKING = 69;
  {$EXTERNALSYM _SC_FILE_LOCKING}
  _SC_GETGR_R_SIZE_MAX = 70;
  {$EXTERNALSYM _SC_GETGR_R_SIZE_MAX}
  _SC_GETPW_R_SIZE_MAX = 71;
  {$EXTERNALSYM _SC_GETPW_R_SIZE_MAX}
  _SC_HOST_NAME_MAX = 72;
  {$EXTERNALSYM _SC_HOST_NAME_MAX}
  _SC_LOGIN_NAME_MAX = 73;
  {$EXTERNALSYM _SC_LOGIN_NAME_MAX}
  _SC_MONOTONIC_CLOCK = 74;
  {$EXTERNALSYM _SC_MONOTONIC_CLOCK}
  _SC_MQ_PRIO_MAX = 75;
  {$EXTERNALSYM _SC_MQ_PRIO_MAX}
  _SC_READER_WRITER_LOCKS = 76;
  {$EXTERNALSYM _SC_READER_WRITER_LOCKS}
  _SC_REGEXP = 77;
  {$EXTERNALSYM _SC_REGEXP}
  _SC_SHELL = 78;
  {$EXTERNALSYM _SC_SHELL}
  _SC_SPAWN = 79;
  {$EXTERNALSYM _SC_SPAWN}
  _SC_SPIN_LOCKS = 80;
  {$EXTERNALSYM _SC_SPIN_LOCKS}
  _SC_SPORADIC_SERVER = 81;
  {$EXTERNALSYM _SC_SPORADIC_SERVER}
  _SC_THREAD_ATTR_STACKADDR = 82;
  {$EXTERNALSYM _SC_THREAD_ATTR_STACKADDR}
  _SC_THREAD_ATTR_STACKSIZE = 83;
  {$EXTERNALSYM _SC_THREAD_ATTR_STACKSIZE}
  _SC_THREAD_CPUTIME = 84;
  {$EXTERNALSYM _SC_THREAD_CPUTIME}
  _SC_THREAD_DESTRUCTOR_ITERATIONS = 85;
  {$EXTERNALSYM _SC_THREAD_DESTRUCTOR_ITERATIONS}
  _SC_THREAD_KEYS_MAX = 86;
  {$EXTERNALSYM _SC_THREAD_KEYS_MAX}
  _SC_THREAD_PRIO_INHERIT = 87;
  {$EXTERNALSYM _SC_THREAD_PRIO_INHERIT}
  _SC_THREAD_PRIO_PROTECT = 88;
  {$EXTERNALSYM _SC_THREAD_PRIO_PROTECT}
  _SC_THREAD_PRIORITY_SCHEDULING = 89;
  {$EXTERNALSYM _SC_THREAD_PRIORITY_SCHEDULING}
  _SC_THREAD_PROCESS_SHARED = 90;
  {$EXTERNALSYM _SC_THREAD_PROCESS_SHARED}
  _SC_THREAD_SAFE_FUNCTIONS = 91;
  {$EXTERNALSYM _SC_THREAD_SAFE_FUNCTIONS}
  _SC_THREAD_SPORADIC_SERVER = 92;
  {$EXTERNALSYM _SC_THREAD_SPORADIC_SERVER}
  _SC_THREAD_STACK_MIN = 93;
  {$EXTERNALSYM _SC_THREAD_STACK_MIN}
  _SC_THREAD_THREADS_MAX = 94;
  {$EXTERNALSYM _SC_THREAD_THREADS_MAX}
  _SC_TIMEOUTS = 95;
  {$EXTERNALSYM _SC_TIMEOUTS}
  _SC_THREADS = 96;
  {$EXTERNALSYM _SC_THREADS}
  _SC_TRACE = 97;
  {$EXTERNALSYM _SC_TRACE}
  _SC_TRACE_EVENT_FILTER = 98;
  {$EXTERNALSYM _SC_TRACE_EVENT_FILTER}
  _SC_TRACE_INHERIT = 99;
  {$EXTERNALSYM _SC_TRACE_INHERIT}
  _SC_TRACE_LOG = 100;
  {$EXTERNALSYM _SC_TRACE_LOG}
  _SC_TTY_NAME_MAX = 101;
  {$EXTERNALSYM _SC_TTY_NAME_MAX}
  _SC_TYPED_MEMORY_OBJECTS = 102;
  {$EXTERNALSYM _SC_TYPED_MEMORY_OBJECTS}
  _SC_V6_ILP32_OFF32 = 103;
  {$EXTERNALSYM _SC_V6_ILP32_OFF32}
  _SC_V6_ILP32_OFFBIG = 104;
  {$EXTERNALSYM _SC_V6_ILP32_OFFBIG}
  _SC_V6_LP64_OFF64 = 105;
  {$EXTERNALSYM _SC_V6_LP64_OFF64}
  _SC_V6_LPBIG_OFFBIG = 106;
  {$EXTERNALSYM _SC_V6_LPBIG_OFFBIG}
  _SC_IPV6 = 118;
  {$EXTERNALSYM _SC_IPV6}
  _SC_RAW_SOCKETS = 119;
  {$EXTERNALSYM _SC_RAW_SOCKETS}
  _SC_SYMLOOP_MAX = 120;
  {$EXTERNALSYM _SC_SYMLOOP_MAX}
  _SC_ATEXIT_MAX = 107;
  {$EXTERNALSYM _SC_ATEXIT_MAX}
  _SC_IOV_MAX = 56;
  {$EXTERNALSYM _SC_IOV_MAX}
  _SC_PAGE_SIZE = _SC_PAGESIZE;
  {$EXTERNALSYM _SC_PAGE_SIZE}
  _SC_XOPEN_CRYPT = 108;
  {$EXTERNALSYM _SC_XOPEN_CRYPT}
  _SC_XOPEN_ENH_I18N = 109;
  {$EXTERNALSYM _SC_XOPEN_ENH_I18N}
  _SC_XOPEN_LEGACY = 110;
  {$EXTERNALSYM _SC_XOPEN_LEGACY}
  _SC_XOPEN_REALTIME = 111;
  {$EXTERNALSYM _SC_XOPEN_REALTIME}
  _SC_XOPEN_REALTIME_THREADS = 112;
  {$EXTERNALSYM _SC_XOPEN_REALTIME_THREADS}
  _SC_XOPEN_SHM = 113;
  {$EXTERNALSYM _SC_XOPEN_SHM}
  _SC_XOPEN_STREAMS = 114;
  {$EXTERNALSYM _SC_XOPEN_STREAMS}
  _SC_XOPEN_UNIX = 115;
  {$EXTERNALSYM _SC_XOPEN_UNIX}
  _SC_XOPEN_VERSION = 116;
  {$EXTERNALSYM _SC_XOPEN_VERSION}
  _SC_XOPEN_XCU_VERSION = 121;
  {$EXTERNALSYM _SC_XOPEN_XCU_VERSION}
  _SC_XBS5_ILP32_OFF32 = 122;
  {$EXTERNALSYM _SC_XBS5_ILP32_OFF32}
  _SC_XBS5_ILP32_OFFBIG = 123;
  {$EXTERNALSYM _SC_XBS5_ILP32_OFFBIG}
  _SC_XBS5_LP64_OFF64 = 124;
  {$EXTERNALSYM _SC_XBS5_LP64_OFF64}
  _SC_XBS5_LPBIG_OFFBIG = 125;
  {$EXTERNALSYM _SC_XBS5_LPBIG_OFFBIG}
  _SC_SS_REPL_MAX = 126;
  {$EXTERNALSYM _SC_SS_REPL_MAX}
  _SC_TRACE_EVENT_NAME_MAX = 127;
  {$EXTERNALSYM _SC_TRACE_EVENT_NAME_MAX}
  _SC_TRACE_NAME_MAX = 128;
  {$EXTERNALSYM _SC_TRACE_NAME_MAX}
  _SC_TRACE_SYS_MAX = 129;
  {$EXTERNALSYM _SC_TRACE_SYS_MAX}
  _SC_TRACE_USER_EVENT_MAX = 130;
  {$EXTERNALSYM _SC_TRACE_USER_EVENT_MAX}
  _SC_PASS_MAX = 131;
  {$EXTERNALSYM _SC_PASS_MAX}

{ configurable pathname variables }
  _PC_LINK_MAX = 1;
  {$EXTERNALSYM _PC_LINK_MAX}
  _PC_MAX_CANON = 2;
  {$EXTERNALSYM _PC_MAX_CANON}
  _PC_MAX_INPUT = 3;
  {$EXTERNALSYM _PC_MAX_INPUT}
  _PC_NAME_MAX = 4;
  {$EXTERNALSYM _PC_NAME_MAX}
  _PC_PATH_MAX = 5;
  {$EXTERNALSYM _PC_PATH_MAX}
  _PC_PIPE_BUF = 6;
  {$EXTERNALSYM _PC_PIPE_BUF}
  _PC_CHOWN_RESTRICTED = 7;
  {$EXTERNALSYM _PC_CHOWN_RESTRICTED}
  _PC_NO_TRUNC = 8;
  {$EXTERNALSYM _PC_NO_TRUNC}
  _PC_VDISABLE = 9;
  {$EXTERNALSYM _PC_VDISABLE}

(* H2PAS - IF Expression: !defined(_POSIX_C_SOURCE) || defined(_DARWIN_C_SOURCE) *)
  _PC_NAME_CHARS_MAX = 10 platform;
  {$EXTERNALSYM _PC_NAME_CHARS_MAX}
  _PC_CASE_SENSITIVE = 11 platform;
  {$EXTERNALSYM _PC_CASE_SENSITIVE}
  _PC_CASE_PRESERVING = 12 platform;
  {$EXTERNALSYM _PC_CASE_PRESERVING}
  _PC_EXTENDED_SECURITY_NP = 13 platform;
  {$EXTERNALSYM _PC_EXTENDED_SECURITY_NP}
  _PC_AUTH_OPAQUE_NP = 14 platform;
  {$EXTERNALSYM _PC_AUTH_OPAQUE_NP}

  _PC_2_SYMLINKS = 15;              { Symlink supported in directory }
  {$EXTERNALSYM _PC_2_SYMLINKS}
  _PC_ALLOC_SIZE_MIN = 16;          { Minimum storage actually allocated }
  {$EXTERNALSYM _PC_ALLOC_SIZE_MIN}
  _PC_ASYNC_IO = 17;                { Async I/O [AIO] supported? }
  {$EXTERNALSYM _PC_ASYNC_IO}
  _PC_FILESIZEBITS = 18;            { # of bits to represent file size }
  {$EXTERNALSYM _PC_FILESIZEBITS}
  _PC_PRIO_IO = 19;                 { Priority I/O [PIO] supported? }
  {$EXTERNALSYM _PC_PRIO_IO}
  _PC_REC_INCR_XFER_SIZE = 20;      { Recommended increment for next two }
  {$EXTERNALSYM _PC_REC_INCR_XFER_SIZE}
  _PC_REC_MAX_XFER_SIZE = 21;       { Recommended max file transfer size }
  {$EXTERNALSYM _PC_REC_MAX_XFER_SIZE}
  _PC_REC_MIN_XFER_SIZE = 22;       { Recommended min file transfer size }
  {$EXTERNALSYM _PC_REC_MIN_XFER_SIZE}
  _PC_REC_XFER_ALIGN = 23;          { Recommended buffer alignment }
  {$EXTERNALSYM _PC_REC_XFER_ALIGN}
  _PC_SYMLINK_MAX = 24;             { Max # of bytes in symlink name }
  {$EXTERNALSYM _PC_SYMLINK_MAX}
  _PC_SYNC_IO = 25;                 { Sync I/O [SIO] supported? }
  {$EXTERNALSYM _PC_SYNC_IO}

type
  TPipeDescriptors = {packed} record
    ReadDes: Integer;
    WriteDes: Integer;
  end;

