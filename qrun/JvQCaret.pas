{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQCaret;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  
  
  QWindows, Qt, QGraphics, QControls, QForms,
  
  SysUtils, Classes,
  JvQTypes;

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
    FUpdateCount: Integer;
    FModified: Boolean;
    FOnChanged: TNotifyEvent;
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
    property UpdateCount: Integer read FUpdateCount;
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
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
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
  JvQJCLUtils, JvQResources;

constructor TJvCaret.Create(Owner: TWinControl);
begin
  if not Assigned(Owner) then
    raise EJVCLException.CreateFmt(RsEInvalidCaretOwner, [ClassName]);
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
  if FUpdateCount = 0 then
  begin
    FModified := False;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end
  else
    FModified := True;
end;

function TJvCaret.UsingBitmap: Boolean;
begin
  Result := (Width = 0) and (Height = 0) and not Gray and not Bitmap.Empty;
end;

function TJvCaret.IsDefaultCaret: Boolean;
begin
  Result := (Width = 0) and (Height = 0) and not Gray and Bitmap.Empty;
end;


type
  TOpenScrollingWidget = class(TScrollingWidget);


procedure TJvCaret.CreateCaret;
const
  GrayHandles: array [Boolean] of THandle = (0, THandle(1));

var
  Handle: QWidgetH;

begin
  if FCaretOwner.Focused and
    not (csDesigning in FCaretOwner.ComponentState) and not IsDefaultCaret then
  begin
    
    
      if FCaretOwner is TScrollingWidget then
        Handle := TOpenScrollingWidget(FCaretOwner).ViewportHandle
      else
        Handle := FCaretOwner.Handle;

      if UsingBitmap then
        OSCheck(QWindows.CreateCaret(Handle, Bitmap.Handle, 0, 0))
      else
      { Gray carets seem to be unsupported on Win95 at least, so if the create
        failed for the gray caret, try again with a standard black caret }
      if not QWindows.CreateCaret(Handle, GrayHandles[Gray], Width, Height) then
        OSCheck(QWindows.CreateCaret(Handle, 0, Width, Height));
      FCaretCreated := True;
      ShowCaret(Handle);
    
  end;
end;

procedure TJvCaret.DestroyCaret;
begin
  if CaretCreated and
    
    not (csDesigning in FCaretOwner.ComponentState) and
    not IsDefaultCaret then
  begin
    
    
    if QWindows.DestroyCaret then
    
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
  if (FUpdateCount = 0) and FModified then
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
