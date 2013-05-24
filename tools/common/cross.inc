{$ifndef fpc}
'This library requires the free pascal compiler'
{$endif}
{$if fpc_fullversion < 20701}
'This library requires the free pascal 2.7.1 or greater'
{$endif}

{$mode delphi}

{$macro on}

{$ifndef fpc_has_constref}
	{$define constref := const}
{$endif}

{$ifdef windows}
	{$define apicall := stdcall}
{$else}
	{$define apicall := cdecl}
{$endif}

{$ifdef linux}
  {$ifdef cpui386}
    {$define linux_i386}
	{$endif}
	{$ifdef cpux86_64}
    {$define linux_x86_64}
  {$endif}
{$endif}


