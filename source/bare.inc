{$ifndef fpc}
'This library requires the free pascal compiler'
{$endif}
{$if fpc_fullversion < 20701}
'This library requires the free pascal 2.7.1 or greater'
{$endif}

{$mode delphi}
{$z4}
{$macro on}

{$ifdef windows}
	{$define apicall := stdcall}
{$else}
	{$define apicall := cdecl}
{$endif}

{$ifdef linux}
  {$ifdef i386}
    {$define linux-i386}
	{$endif}
	{$ifdef cpux86_64}
    {$define linux-x86_64}
  {$endif}
{$endif}

