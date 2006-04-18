{******************************************************************************}
{*                                                                            *}
{* PasDesigner 0.1 - Utils                                                    *}
{*                                                                            *}
{* (C) 2003-2004 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit PDHelpers;

{$I jedi.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF}
  SysUtils;

type
  ICriticalSectionBlock = interface
    ['{7AE55097-5987-4AE6-A186-48500252F093}']
  end;

function CSBlock(CS: TRTLCriticalSection): ICriticalSectionBlock;
  // enters CS and leaves it in the destructor that is called by compiler magic

function UpName(const Name: string): string;
  // returns Name with the first char in uppercase and the rest in lower case

implementation

type
  TCriticalSectionBlock = class(TInterfacedObject, ICriticalSectionBlock)
  public
    FCS: TRTLCriticalSection;
    constructor Create(ACS: TRTLCriticalSection);
    destructor Destroy; override;
  end;

{ TCriticalSectionBlock }

constructor TCriticalSectionBlock.Create(ACS: TRTLCriticalSection);
begin
  inherited Create;
  FCS := ACS;
  EnterCriticalSection(FCS);
end;

destructor TCriticalSectionBlock.Destroy;
begin
  LeaveCriticalSection(FCS);
  inherited Destroy;
end;


function CSBlock(CS: TRTLCriticalSection): ICriticalSectionBlock;
begin
  Result := TCriticalSectionBlock.Create(CS);
end;

function UpName(const Name: string): string;
begin
  if Name <> '' then
    Result := Name[1] + AnsiLowerCase(Copy(Name, 2, MaxInt))
  else
    Result := '';
end;

end.
