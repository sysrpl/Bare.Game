(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.interop.posix.txt> }
unit Bare.Interop.Posix;

{$i bare.inc}

interface

{$ifdef unix}
  {$ifdef linux}
    {$define libc := external 'libc.so'}
  {$endif}
const
  S_IRUSR = $0100;
  S_IWUSR = $0080;
  S_IXUSR = $0040;
  S_IAUSR = S_IRUSR or S_IWUSR or S_IXUSR;

function rename(oldname, newname: PChar): LongInt; cdecl; libc;
function unlink(path: PChar): LongInt; cdecl; libc;
function getcwd(buf: PChar; size: IntPtr): PChar; cdecl; libc;
function chdir(path: PChar): LongInt; cdecl; libc;
function rmdir(path: PChar): LongInt; cdecl; libc;
function mkdir(path: PChar; mode: LongWord): LongInt; cdecl; libc;
{$endif}

implementation

end.

