{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCaret.PAS, released on 2003-02-15.

The Initial Developer of the Original Code is Joe Doe .
Portions created by Joe Doe are Copyright (C) 1999 Joe Doe.
Portions created by XXXX Corp. are Copyright (C) 1998, 1999 XXXX Corp.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCaret;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls,
  JVCLVer, JvTypes;

type
  { A caret can be specified either by giving a bitmap that defines its shape
    or by defining the caret width and height. If a bitmap is specified the
    other properties are set to 0, if width or height are specified the
    bitmap is not used. A change to the caret at runtime will only have an
    immediate effect if the control has focus. }

  TJvCaret = class(TPersistent)
  private
    FCaretBitmap: TBitmap;
    FCaretWidth: Integer;
    FCaretHeight: Integer;
    FGrayCaret: Boolean;
    FCaretOwner: TWinControl;
    FUpdatecount: Integer;
    FChangedEvent: TNotifyEvent;
    FCaretCreated: Boolean;
    procedure SetCaretBitmap(const Value: TBitmap);
    procedure SetCaretHeight(const Value: Integer);
    procedure SetCaretWidth(const Value: Integer);
    procedure SetGrayCaret(const Value: Boolean);
    procedure ReadBitmap(Stream: TStream);
    procedure WriteBitmap(Stream: TStream);
  protected
    procedure Changed; dynamic;
    function UsingBitmap: Boolean;
    function IsDefaultCaret: Boolean;
    property CaretOwner: TWinControl read FCaretOwner;
    property Updatecount: Integer read FUpdatecount;
    property CaretCreated: Boolean read FCaretCreated;
  public
    constructor Create(Owner: TWinControl);
    destructor Destroy; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure CreateCaret;
    procedure DestroyCaret;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property OnChanged: TNotifyEvent read FChangedEvent write FChangedEvent;
  published
    { Note: streaming system does not deal properly with a published persistent
      property on another nested persistent. We use a pseudoproperty to save the
      bitmap. }
    property Bitmap: TBitmap read FCaretBitmap write SetCaretBitmap stored False;
    property Width: Integer read FCaretWidth write SetCaretWidth default 0;
    property Height: Integer read FCaretHeight write SetCaretHeight default 0;
    property Gray: Boolean read FGrayCaret write SetGrayCaret default False;
  end;

implementation

uses
  JvFunctions;

constructor TJvCaret.Create(Owner: TWinControl);
begin
  if not Assigned(Owner) then
    raise EJVCLException.Create('TJvCaret.Create: cannot be created without valid Owner');
  inherited Create;
  FCaretOwner := Owner;
  FCaretBitmap := TBitmap.Create;
end;

destructor TJvCaret.Destroy;
begin
  DestroyCaret;
  FCaretBitmap.Free;
  inherited Destroy;
end;

procedure TJvCaret.Assign(Source: TPersistent);
begin
  if Source is TJvCaret then
  begin
    BeginUpdate;
    try
      FCaretWidth := TJvCaret(Source).Width;
      FCaretHeight := TJvCaret(Source).Height;
      FGrayCaret := TJvCaret(Source).Gray;
      Bitmap := TJvCaret(Source).Bitmap;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvCaret.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TJvCaret.Changed;
begin
  if Assigned(OnChanged) and (FUpdateCount = 0) then
    OnChanged(Self);
end;

function TJvCaret.UsingBitmap: Boolean;
begin
  Result := (Width = 0) and (Height = 0) and not Gray and not Bitmap.Empty;
end;

function TJvCaret.IsDefaultCaret: Boolean;
begin
  Result := (Width = 0) and (Height = 0) and not Gray and Bitmap.Empty;
end;

procedure TJvCaret.CreateCaret;
const
  GrayHandles: array [Boolean] of THandle = (0, THandle(1));
begin
  if FCaretOwner.Focused and
    not (csDesigning in FCaretOwner.ComponentState) and not IsDefaultCaret then
  begin
    if UsingBitmap then
      OSCheck(Windows.CreateCaret(FCaretOwner.handle, Bitmap.Handle, 0, 0))
    else
    { Gray carets seem to be unsupported on Win95 at least, so if the create
      failed for the gray caret, try again with a standard black caret }
    if not Windows.CreateCaret(FCaretOwner.handle, GrayHandles[Gray], Width, Height) then
      OSCheck(Windows.CreateCaret(FCaretOwner.handle, 0, Width, Height));
    FCaretCreated := True;
    ShowCaret(FCaretOwner.handle);
  end;
end;

procedure TJvCaret.DestroyCaret;
begin
  if CaretCreated and FCaretOwner.Focused and not (csDesigning in FCaretOwner.ComponentState) and
    not IsDefaultCaret then
  begin
    if Windows.DestroyCaret then
      FCaretCreated := False;
  end;
end;

procedure TJvCaret.ReadBitmap(Stream: TStream);
begin
  FCaretBitmap.LoadFromStream(Stream);
end;

procedure TJvCaret.WriteBitmap(Stream: TStream);
begin
  FCaretBitmap.SaveToStream(Stream);
end;

procedure TJvCaret.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('CaretBitmap', ReadBitmap,
    WriteBitmap, not FCaretBitmap.Empty);
end;

procedure TJvCaret.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed;
end;

procedure TJvCaret.SetCaretBitmap(const Value: TBitmap);
begin
  FCaretBitmap.Assign(Value);
  FCaretWidth := 0;
  FCaretHeight := 0;
  FGrayCaret := False;
  Changed;
end;

procedure TJvCaret.SetCaretHeight(const Value: Integer);
begin
  if FCaretHeight <> Value then
  begin
    FCaretHeight := Value;
    Changed;
  end;
end;

procedure TJvCaret.SetCaretWidth(const Value: Integer);
begin
  if FCaretWidth <> Value then
  begin
    FCaretWidth := Value;
    Changed;
  end;
end;

procedure TJvCaret.SetGrayCaret(const Value: Boolean);
begin
  if FGrayCaret <> Value then
  begin
    FGrayCaret := Value;
    Changed;
  end;
end;

end.
