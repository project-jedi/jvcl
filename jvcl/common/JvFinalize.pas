{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFinalize.pas, released on 2004-02-27.

The Initial Developers of the Original Code is: Andreas Hausladen
Copyright (c) 2004 Andreas Hausladen
All Rights Reserved.

Last Modified: 2004-02-27

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

The purpose of this unit is to reduce code inclusion for functions that are not
used by the program and which are only included because they are used in the
finalization section.

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvFinalize;

interface

type
  TFinalizeProc = procedure;

/// <summary>
/// AddFinalizeProc adds a TFinalizeProc to the finalize section.
/// The procedure is called on finalization.
/// </summary>
procedure AddFinalizeProc(FinalizeProc: TFinalizeProc);

/// <summary>
/// AddFinalizeObject adds an TObject derived class to the finalize section.
/// The object is destroyed on finalization.
/// </summary>
function AddFinalizeObject(Instance: TObject): TObject;

/// <summary>
/// AddFinalizeObjectNil adds an TObject derived class to the finalize section.
/// The object is destroyed and the reference is set to nil on finalization.
/// </summary>
/// <limitation>
/// Only global variables are allowed to be specified.
/// </limitation>
function AddFinalizeObjectNil(var Reference: TObject): TObject;

/// <summary>
/// AddFinalizeMemory adds an memory allocation to the finalize section.
/// The memory is released on finalization.
/// </summary>
function AddFinalizeMemory(Ptr: Pointer): Pointer;

/// <summary>
/// AddFinalizeMemory adds an memory allocation to the finalize section.
/// The memory is released and the Ptr is set to nil on finalization.
/// </summary>
/// <limitation>
/// Only global variables are allowed to be specified.
/// </limitation>
function AddFinalizeMemoryNil(var Ptr: Pointer): Pointer;

implementation

type
  TFinalizeItem = class(TObject)
  private
    FNext: TFinalizeItem;
  public
    constructor Create(ANext: TFinalizeItem);

    property Next: TFinalizeItem read FNext;
  end;

{ TFinalizeItem }

constructor TFinalizeItem.Create(ANext: TFinalizeItem);
begin
  inherited Create;
  FNext := ANext;
end;

var
  FinalizeList: TFinalizeItem = nil;

procedure FinalizeUnits;
var
  P: TFinalizeItem;
begin
  while FinalizeList <> nil do
  begin
    P := FinalizeList;
    FinalizeList := P.Next;
    try
      P.Free;
    except
      // ignore
    end;
  end;
end;


// -----------------------------------------------------------------------------
type
  TFinalizeProcItem = class(TFinalizeItem)
  private
    FFinalizeProc: TFinalizeProc;
  public
    constructor Create(AFinalizeProc: TFinalizeProc; ANext: TFinalizeItem);
    destructor Destroy; override;
  end;

  TFinalizeObjectItem = class(TFinalizeItem)
  private
    FInstance: TObject;
  public
    constructor Create(AInstance: TObject; ANext: TFinalizeItem);
    destructor Destroy; override;
  end;

  TFinalizeObjectNilItem = class(TFinalizeItem)
  private
    FReference: ^TObject;
  public
    constructor Create(var AReference: TObject; ANext: TFinalizeItem);
    destructor Destroy; override;
  end;

  TFinalizeMemoryItem = class(TFinalizeItem)
  private
    FPtr: Pointer;
  public
    constructor Create(APtr: Pointer; ANext: TFinalizeItem);
    destructor Destroy; override;
  end;

  TFinalizeMemoryNilItem = class(TFinalizeItem)
  private
    FPtr: ^Pointer;
  public
    constructor Create(var APtr: Pointer; ANext: TFinalizeItem);
    destructor Destroy; override;
  end;

{ TFinalizeProcItem }

constructor TFinalizeProcItem.Create(AFinalizeProc: TFinalizeProc; ANext: TFinalizeItem);
begin
  inherited Create(ANext);
  FFinalizeProc := AFinalizeProc;
end;

destructor TFinalizeProcItem.Destroy;
begin
  FFinalizeProc();
  inherited Destroy;
end;


{ TFinalizeObjectItem }

constructor TFinalizeObjectItem.Create(AInstance: TObject; ANext: TFinalizeItem);
begin
  inherited Create(ANext);
  FInstance := AInstance;
end;

destructor TFinalizeObjectItem.Destroy;
begin
  FInstance.Free;
  inherited Destroy;
end;


{ TFinalizeObjectNilItem }

constructor TFinalizeObjectNilItem.Create(var AReference: TObject; ANext: TFinalizeItem);
begin
  inherited Create(ANext);
  FReference := @AReference;
end;

destructor TFinalizeObjectNilItem.Destroy;
begin
  FReference^.Free;
  FReference^ := nil;
  inherited Destroy;
end;



procedure AddFinalizeProc(FinalizeProc: TFinalizeProc);
begin
  FinalizeList := TFinalizeProcItem.Create(FinalizeProc, FinalizeList);
end;

function AddFinalizeObject(Instance: TObject): TObject;
begin
  FinalizeList := TFinalizeObjectItem.Create(Instance, FinalizeList);
  Result := Instance;
end;

function AddFinalizeObjectNil(var Reference: TObject): TObject;
begin
  FinalizeList := TFinalizeObjectNilItem.Create(Reference, FinalizeList);
  Result := Reference;
end;

function AddFinalizeMemory(Ptr: Pointer): Pointer;
begin
  FinalizeList := TFinalizeMemoryItem.Create(Ptr, FinalizeList);
  Result := Ptr;
end;

function AddFinalizeMemoryNil(var Ptr: Pointer): Pointer;
begin
  FinalizeList := TFinalizeMemoryNilItem.Create(Ptr, FinalizeList);
  Result := Ptr;
end;


{ TFinalizeMemoryItem }

constructor TFinalizeMemoryItem.Create(APtr: Pointer; ANext: TFinalizeItem);
begin
  inherited Create(ANext);
  FPtr := APtr;
end;

destructor TFinalizeMemoryItem.Destroy;
begin
  if FPtr <> nil then
    FreeMem(FPtr);
  inherited Destroy;
end;


{ TFinalizeMemoryNilItem }

constructor TFinalizeMemoryNilItem.Create(var APtr: Pointer;
  ANext: TFinalizeItem);
begin
  inherited Create(ANext);
  FPtr := @APtr;
end;

destructor TFinalizeMemoryNilItem.Destroy;
begin
  if FPtr^ <> nil then
  begin
    FreeMem(FPtr^);
    FPtr^ := nil;
  end;
  inherited Destroy;
end;

initialization

finalization
  FinalizeUnits;

end.
