{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$IFDEF COMPILER6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}

{== unit CaretEdit ====================================================}
{: Implements a TEdit and TMemo derivative that has a configurable caret.
@author Dr. Peter Below
@desc   Version 1.0 created 20 Oktober 2000<BR>
        Current revision 1.0<BR>
        Last modified       20 Oktober 2000<P>
Install this unit via Component->Install component into a run-time package}
{======================================================================}
unit JvCaretEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JVCLVer;

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
    FCaretOwner: TCustomEdit;
    FUpdatecount: Integer;
    FChangedEvent: TNotifyEvent;
    procedure SetCaretBitmap(const Value: TBitmap);
    procedure SetCaretHeight(const Value: Integer);
    procedure SetCaretWidth(const Value: Integer);
    procedure SetGrayCaret(const Value: Boolean);
    procedure ReadBitmap(Stream: TStream);
    procedure WriteBitmap(Stream: TStream);
  protected
    procedure Changed; dynamic;
    property CaretOwner: TCustomEdit read FCaretOwner;
    property Updatecount: Integer read FUpdatecount;
  public
    constructor Create(owner: TCustomEdit);
    destructor Destroy; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure CreateCaret;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property OnChanged: TNotifyEvent read FChangedEvent write FChangedEvent;
  published
    { Note: streaming system does not deal properly with a published persistent
      property on another nested persitent. We use a pseudoproperty to save the
      bitmap. }
    property Bitmap: TBitmap read FCaretBitmap write SetCaretBitmap stored false;
    property Width: Integer read FCaretWidth write SetCaretWidth default 0;
    property Height: Integer read FCaretHeight write SetCaretHeight default 0;
    property Gray: Boolean read FGrayCaret write SetGrayCaret default false;
  end;

  TJvCaretEdit = class(TEdit)
  private
    { Private declarations }
    FCaret: TJvCaret;
    procedure SetCaret(const Value: TJvCaret);
    procedure CaretChanged(sender: TObject); dynamic;
    procedure WMSetFocus(var msg: TMessage); message WM_SETFOCUS;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Caret: TJvCaret read FCaret write SetCaret;
  end;

  TJvCaretMemo = class(TMemo)
  private
    { Private declarations }
    FCaret: TJvCaret;
    FAboutJVCL: TJVCLAboutInfo;
    procedure CaretChanged(sender: TObject); dynamic;
    procedure SetCaret(const Value: TJvCaret);
    procedure WMSetFocus(var msg: TMessage); message WM_SETFOCUS;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Caret: TJvCaret read FCaret write SetCaret;
  end;

implementation
uses
  JvComponentFunctions;

{ TJvCaret }

procedure TJvCaret.Assign(Source: TPersistent);
begin
  if source is TJvCaret then
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
    inherited;
end;

procedure TJvCaret.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TJvCaret.Changed;
begin
  if Assigned(OnChanged) and (FUpdatecount = 0) then
    OnChanged(self);
end;

constructor TJvCaret.Create(owner: TCustomEdit);
begin
  Assert(Assigned(owner));
  inherited Create;
  FCaretOwner := owner;
  FCaretBitmap := TBitmap.Create;
end;

procedure TJvCaret.CreateCaret;
  function UsingBitmap: Boolean;
  begin
    result := (Width = 0) and (Height = 0) and not Gray
      and not Bitmap.Empty;
  end;
  function IsDefaultCaret: Boolean;
  begin
    Result := (Width = 0) and (Height = 0) and not Gray
      and Bitmap.Empty;
  end;
const
  GrayHandles: array[Boolean] of THandle = (0, THandle(-1));
begin
  if FCaretOwner.Focused
    and not (csDesigning in FCaretOwner.ComponentState)
    and not IsDefaultCaret then
  begin
    if UsingBitmap then
      OSCheck(Windows.CreateCaret(FCaretOwner.handle, Bitmap.Handle, 0, 0))
    else if not Windows.CreateCaret(FCaretOwner.handle, GrayHandles[Gray],
      Width, Height) then
      Windows.CreateCaret(FCaretOwner.handle, 0, Width, Height);
    { Gray carets seem to be unsupported on Win95 at least, so if the create
      failed for the gray caret, try again with a standard black caret }

    ShowCaret(FCaretOwner.handle);
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

destructor TJvCaret.Destroy;
begin
  FCaretBitmap.Free;
  inherited;
end;

procedure TJvCaret.EndUpdate;
begin
  Dec(FUpdatecount);
  Changed;
end;

procedure TJvCaret.SetCaretBitmap(const Value: TBitmap);
begin
  FCaretBitmap.Assign(value);
  FCaretWidth := 0;
  FCaretHeight := 0;
  FGrayCaret := false;
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

{ TJvCaretEdit }

procedure TJvCaretEdit.CaretChanged(sender: TObject);
begin
  FCaret.CreateCaret;
end;

constructor TJvCaretEdit.Create(aOwner: TComponent);
begin
  inherited;
  FCaret := TJvCaret.Create(self);
  FCaret.OnChanged := CaretChanged;
end;

destructor TJvCaretEdit.Destroy;
begin
  FCaret.Free;
  inherited;
end;

procedure TJvCaretEdit.SetCaret(const Value: TJvCaret);
begin
  FCaret.Assign(Value);
end;

procedure TJvCaretEdit.WMSetFocus(var msg: TMessage);
begin
  inherited;
  FCaret.CreateCaret;
end;

{ TJvCaretMemo }

procedure TJvCaretMemo.CaretChanged(sender: TObject);
begin
  FCaret.CreateCaret;
end;

constructor TJvCaretMemo.Create(aOwner: TComponent);
begin
  inherited;
  FCaret := TJvCaret.Create(self);
  FCaret.OnChanged := CaretChanged;
end;

destructor TJvCaretMemo.Destroy;
begin
  FCaret.Free;
  inherited;
end;

procedure TJvCaretMemo.SetCaret(const Value: TJvCaret);
begin
  FCaret.Assign(Value);
end;

procedure TJvCaretMemo.WMSetFocus(var msg: TMessage);
begin
  inherited;
  FCaret.CreateCaret;
end;

end.
