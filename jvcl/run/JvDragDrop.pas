{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDragDrop.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com],
                Andreas Hausladen [Andreas dott Hausladen att gmx dott de].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvDragDrop;

interface

uses
  Windows, Messages, ShellAPI, ActiveX, Classes, Controls,
  JvComponent;

type
  TJvDropTarget = class;
  TJvDragDrop = class;

  TJvDropEvent = procedure(Sender: TObject; Pos: TPoint; Value: TStrings) of object;
  TJvDropEffect = (deNone, deCopy, deMove, deLink, deScroll);

  TJvDragEvent = procedure(Sender: TJvDropTarget; var Effect: TJvDropEffect) of object;
  TJvDragDropEvent = procedure(Sender: TJvDropTarget; var Effect: TJvDropEffect;
    Shift: TShiftState; X, Y: Integer) of object;
  TJvDragLeaveEvent = procedure(Sender: TJvDropTarget) of object;
  TJvDragAcceptEvent = procedure(Sender: TJvDropTarget; var Accept: Boolean) of object;

  TJvDropTarget = class(TJvComponent, IDropTarget)
  private
    FDataObject: IDataObject;
    FStreamedAcceptDrag: Boolean;
    FControl: TWinControl;
    FOnDragDrop: TJvDragDropEvent;
    FOnDragAccept: TJvDragAcceptEvent;
    FOnDragEnter: TJvDragEvent;
    FOnDragOver: TJvDragEvent;
    FOnDragLeave: TJvDragLeaveEvent;
    FAcceptDrag: Boolean;
    procedure SetControl(Value: TWinControl);
    procedure SetAcceptDrag(Value: Boolean);
    procedure RegisterControl;
    procedure UnregisterControl;
  protected
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HRESULT; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HRESULT; stdcall;
    function DragLeave: HRESULT; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HRESULT; stdcall;
    function DoDragAccept: Boolean; dynamic;
    procedure DoDragEnter(var Effect: Longint); dynamic;
    procedure DoDragOver(var Effect: Longint); dynamic;
    procedure DoDragLeave; dynamic;
    procedure DoDragDrop(var Effect: Longint; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFilenames(List: TStrings): Integer;
    // GetFilenames returns the HDROP Filenames. (same as TJvDragDrop).
    // Return value: number of filenames
    function GetFileDescrNames(List: TStrings): Integer;
    // GetFileDescrNames returns the File Descriptor file names (not available for Explorer drag/drop)
    function GetFileDescrCount: Integer;
    // GetFileDescrCount returns the number of File Descroptor file names.
    function GetFileContent(Index: Integer; Stream: TStream): Boolean;
    // GetFileContent returns the file content of the File Descriptor
    property DataObject: IDataObject read FDataObject;
  published
    property AcceptDrag: Boolean read FAcceptDrag write SetAcceptDrag default True;
    property Control: TWinControl read FControl write SetControl;
    property OnDragDrop: TJvDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnDragAccept: TJvDragAcceptEvent read FOnDragAccept write FOnDragAccept;
    property OnDragEnter: TJvDragEvent read FOnDragEnter write FOnDragEnter;
    property OnDragOver: TJvDragEvent read FOnDragOver write FOnDragOver;
    property OnDragLeave: TJvDragLeaveEvent read FOnDragLeave write FOnDragLeave;
  end;

  TJvDragDrop = class(TJvComponent)
  private
    FAcceptDrag: Boolean;
    FStreamedAcceptDrag: Boolean;
    FFiles: TStringList;
    FOnDrop: TJvDropEvent;
    FIsHooked: Boolean;
    FTargetStrings: TStrings;
    FDropTarget: TWinControl;
    procedure DropFiles(Handle: HDROP);
    function GetFiles: TStrings;
    procedure SetAcceptDrag(Value: Boolean);
    procedure SetDropTarget(const Value: TWinControl);
    function WndProc(var Msg: TMessage): Boolean;
  protected
    procedure HookControl;
    procedure UnHookControl;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Files: TStrings read GetFiles;
    property TargetStrings: TStrings read FTargetStrings write FTargetStrings;
  published
    property AcceptDrag: Boolean read FAcceptDrag write SetAcceptDrag default True;
    property DropTarget: TWinControl read FDropTarget write SetDropTarget;
    property OnDrop: TJvDropEvent read FOnDrop write FOnDrop;
  end;

function CF_FILEDESCRIPTOR: DWORD;
function CF_FILECONTENTS: DWORD;
function Malloc: IMalloc;

implementation

uses
  ShlObj, SysUtils, Forms,
  {$IFDEF COMPILER5}
  JvJCLUtils,
  {$ENDIF COMPILER5}
  JvWndProcHook;

var
  GlobalCF_FILEDESCRIPTOR: DWORD = $FFFFFFF;
  GlobalCF_FILECONTENTS: DWORD = $FFFFFFF;
  GlobalMalloc: IMalloc = nil;

  FileDropFormatEtc: FORMATETC;
  FileContentFormatEtc: FORMATETC;
  FileDescriptorFormatEtc: FORMATETC;

function CF_FILEDESCRIPTOR: DWORD;
begin
  if GlobalCF_FILEDESCRIPTOR = $FFFFFFF then
    GlobalCF_FILEDESCRIPTOR := RegisterClipboardFormat(CFSTR_FILEDESCRIPTOR);
  Result := GlobalCF_FILEDESCRIPTOR;
end;

function CF_FILECONTENTS: DWORD;
begin
  if GlobalCF_FILECONTENTS = $FFFFFFF then
    GlobalCF_FILECONTENTS := RegisterClipboardFormat(CFSTR_FILECONTENTS);
  Result := GlobalCF_FILECONTENTS;
end;

function Malloc: IMalloc;
begin
  if not Assigned(GlobalMalloc) then
    ShGetMalloc(GlobalMalloc);
  Result := GlobalMalloc;
end;

//=== { TJvDragDrop } ========================================================

constructor TJvDragDrop.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptDrag := False;
  FStreamedAcceptDrag := True;
  FFiles := TStringList.Create;
  FIsHooked := False;
  if (Owner is TWinControl) and (csDesigning in ComponentState) then
    FDropTarget := TWinControl(Owner);
end;

destructor TJvDragDrop.Destroy;
begin
  UnHookControl;
  FFiles.Free;
  inherited Destroy;
end;

procedure TJvDragDrop.Loaded;
begin
  inherited Loaded;
  SetAcceptDrag(FStreamedAcceptDrag);
end;

procedure TJvDragDrop.DropFiles(Handle: HDROP);
var
  Buffer: PChar;
  I, BufferLength, NeededLength: Integer;
  MousePt: TPoint;
  Count: Integer;
begin
  FFiles.Clear;

  BufferLength := MAX_PATH;

  { Note: Do not use fixed stack buffers of size MAX_PATH,
          to prevent buffer overrun attacks, be paranoid <g> }
  GetMem(Buffer, BufferLength);
  try
    { Return value is a count of the dropped files }
    Count := DragQueryFile(Handle, $FFFFFFFF, nil, 0);

    for I := 0 to Count-1 do
    begin
      { Return value is the required size, in characters, of the buffer,
        *not* including the terminating null character (hence the + 1) }
      NeededLength := DragQueryFile(Handle, I, nil, 0) + 1;
      if NeededLength > BufferLength then
      begin
        BufferLength := NeededLength;
        ReallocMem(Buffer, BufferLength);
      end;
      DragQueryFile(Handle, I, Buffer, BufferLength);
      FFiles.Add(Buffer);
    end;
  finally
    FreeMem(Buffer);
  end;

  if Assigned(FTargetStrings) then
    FTargetStrings.Assign(FFiles);

  if Assigned(FOnDrop) then
  begin
    DragQueryPoint(Handle, MousePt);
    FOnDrop(Self, MousePt, FFiles);
  end;

  DragFinish(Handle);
end;

procedure TJvDragDrop.HookControl;
begin
  if not FIsHooked then
    { Paranoia checks }
    if Assigned(FDropTarget) and not (csDesigning in ComponentState) then
      FIsHooked := RegisterWndProcHook(FDropTarget, WndProc, hoBeforeMsg);
end;

procedure TJvDragDrop.UnHookControl;
begin
  if FIsHooked then
  begin
    FIsHooked := False;
    { Paranoia checks }
    if Assigned(FDropTarget) and not (csDesigning in ComponentState) then
      UnRegisterWndProcHook(FDropTarget, WndProc, hoBeforeMsg);
  end;
end;

procedure TJvDragDrop.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (AComponent = FDropTarget) and (Operation = opRemove) then
    DropTarget := nil;
end;

procedure TJvDragDrop.SetAcceptDrag(Value: Boolean);
begin
  if csLoading in ComponentState then
    { When loading, delay changing to active until all properties are loaded }
    FStreamedAcceptDrag := Value
  else
  if Value <> FAcceptDrag then
  begin
    FAcceptDrag := Value;

    if Assigned(FDropTarget) and not (csDesigning in ComponentState) then
    begin
      { If the component is being destroyed, we don't want to call its Handle
        property, which will implicitly re-create its already destroyed handle }
      if not (csDestroying in FDropTarget.ComponentState) then
        DragAcceptFiles(FDropTarget.Handle, FAcceptDrag);

      if FAcceptDrag then
        HookControl
      else
        UnHookControl;
    end;
  end;
end;

function TJvDragDrop.GetFiles: TStrings;
begin
  Result := FFiles;
end;

procedure TJvDragDrop.SetDropTarget(const Value: TWinControl);
var
  WasActive: Boolean;
begin
  if csLoading in ComponentState then
    FDropTarget := Value
  else
  if Value <> FDropTarget then
  begin
    WasActive := AcceptDrag;

    { This will implicitly unhook the current DropTarget }
    AcceptDrag := False;

    if Assigned(FDropTarget) then
      FDropTarget.RemoveFreeNotification(Self);

    FDropTarget := Value;

    if Assigned(FDropTarget) then
      FDropTarget.FreeNotification(Self);

    if WasActive then
      { And hook again.. }
      AcceptDrag := True;
  end;
end;

function TJvDragDrop.WndProc(var Msg: TMessage): Boolean;
begin
  Result := Msg.Msg = WM_DROPFILES;
  if Result then
    DropFiles(HDROP(Msg.WParam));
end;


//=== { TJvDropTarget } ======================================================

procedure InitFormatEtc;
begin
  if FileDescriptorFormatEtc.cfFormat <> 0 then
    Exit;

  FileDropFormatEtc.cfFormat := CF_HDROP;
  FileDropFormatEtc.ptd := nil;
  FileDropFormatEtc.dwAspect := DVASPECT_CONTENT;
  FileDropFormatEtc.lindex := 0;
  FileDropFormatEtc.tymed := TYMED_HGLOBAL;

  FileDescriptorFormatEtc.cfFormat := CF_FILEDESCRIPTOR;
  FileDescriptorFormatEtc.ptd := nil;
  FileDescriptorFormatEtc.dwAspect := DVASPECT_CONTENT;
  FileDescriptorFormatEtc.lindex := -1;
  FileDescriptorFormatEtc.tymed := TYMED_HGLOBAL;

  FileContentFormatEtc.cfFormat := CF_FILECONTENTS;
  FileContentFormatEtc.ptd := nil;
  FileContentFormatEtc.dwAspect := DVASPECT_CONTENT;
  FileContentFormatEtc.lindex := 0;
  FileContentFormatEtc.tymed := TYMED_ISTREAM;
end;

procedure GetDropEffect(Effect: Longint; var Eff: TJvDropEffect);
begin
  Eff := deNone;
  if (Effect and DROPEFFECT_NONE) <> 0 then
    Eff := deNone
  else
  if (Effect and DROPEFFECT_COPY) <> 0 then
    Eff := deCopy
  else
  if (Effect and DROPEFFECT_MOVE) <> 0 then
    Eff := deMove
  else
  if (Effect and DROPEFFECT_LINK) <> 0 then
    Eff := deLink
  else
  if (Effect and DROPEFFECT_SCROLL) <> 0 then
    Eff := deScroll;
end;

procedure SetDropEffect(var Effect: Longint; Eff: TJvDropEffect);
begin
  case Eff of
    deNone:
      Effect := DROPEFFECT_NONE;
    deCopy:
      Effect := DROPEFFECT_COPY;
    deMove:
      Effect := DROPEFFECT_MOVE;
    deLink:
      Effect := DROPEFFECT_LINK;
    deScroll:
      Effect := Longint(DROPEFFECT_SCROLL);
  end;
end;

constructor TJvDropTarget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Malloc; // a simple call prevents Delphi from crashing

  FAcceptDrag := True;
  FStreamedAcceptDrag := True;

  InitFormatEtc;
end;

destructor TJvDropTarget.Destroy;
begin
  UnregisterControl;
  FDataObject := nil;
  inherited Destroy;
end;

function TJvDropTarget.DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
  pt: TPoint; var dwEffect: Longint): HRESULT;
begin
  FDataObject := dataObj;
  Result := S_OK;

  if not DoDragAccept then
  begin
    FDataObject := nil;
    dwEffect := DROPEFFECT_NONE;
  end
  else
  begin
    dwEffect := DROPEFFECT_COPY;
    try
      DoDragEnter(dwEffect);
    except
      Result := E_UNEXPECTED;
    end;
  end;
end;

function TJvDropTarget.DragOver(grfKeyState: Longint; pt: TPoint;
  var dwEffect: Longint): HRESULT;
begin
  Result := S_OK;
  if FDataObject = nil then
  begin
    FDataObject := nil;
    dwEffect := DROPEFFECT_NONE;
  end
  else
  begin
    dwEffect := DROPEFFECT_COPY;
    try
      DoDragOver(dwEffect);
    except
      Result := E_UNEXPECTED;
    end;
  end;
end;

function TJvDropTarget.DragLeave: HRESULT;
begin
  try
    DoDragLeave;
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
  FDataObject := nil;
end;

function TJvDropTarget.Drop(const dataObj: IDataObject; grfKeyState: Longint;
  pt: TPoint; var dwEffect: Longint): HRESULT;
begin
  Result := S_OK;
  if FDataObject = nil then
  begin
    FDataObject := nil;
    dwEffect := DROPEFFECT_NONE;
  end
  else
  begin
    dwEffect := DROPEFFECT_COPY;
    try
      DoDragDrop(dwEffect, KeyDataToShiftState(grfKeyState), pt.X, pt.Y);
    except
      Result := E_UNEXPECTED;
    end;
    FDataObject := nil;
  end;
end;

function TJvDropTarget.DoDragAccept: Boolean;
begin
  Result := True;
  if Assigned(FOnDragAccept) then
    FOnDragAccept(Self, Result);
end;

procedure TJvDropTarget.DoDragEnter(var Effect: Longint);
var
  Eff: TJvDropEffect;
begin
  GetDropEffect(Effect, Eff);
  if Assigned(FOnDragEnter) then
    FOnDragEnter(Self, Eff);
  SetDropEffect(Effect, Eff);
end;

procedure TJvDropTarget.DoDragOver(var Effect: Longint);
var
  Eff: TJvDropEffect;
begin
  GetDropEffect(Effect, Eff);
  if Assigned(FOnDragOver) then
    FOnDragOver(Self, Eff);
  SetDropEffect(Effect, Eff);
end;

procedure TJvDropTarget.DoDragLeave;
begin
  if Assigned(FOnDragLeave) then
    FOnDragLeave(Self);
end;

procedure TJvDropTarget.DoDragDrop(var Effect: Longint; Shift: TShiftState;
  X, Y: Integer);
var
  Eff: TJvDropEffect;
begin
  GetDropEffect(Effect, Eff);
  if Assigned(FOnDragDrop) then
    FOnDragDrop(Self, Eff, Shift, X, Y);
  SetDropEffect(Effect, Eff);
end;

procedure TJvDropTarget.SetControl(Value: TWinControl);
begin
  if Value <> FControl then
  begin
    UnregisterControl;
    if Assigned(FControl) then
      FControl.RemoveFreeNotification(Self);

    FControl := Value;

    if Assigned(FControl) then
      FControl.FreeNotification(Self);
      
    RegisterControl;
  end;
end;

procedure TJvDropTarget.RegisterControl;
begin
  if FAcceptDrag and Assigned(FControl) and not (csDesigning in ComponentState) then
  begin
    if RegisterDragDrop(FControl.Handle, Self) <> S_OK then
      RaiseLastOSError;
  end;
end;

procedure TJvDropTarget.UnregisterControl;
begin
  if FAcceptDrag and Assigned(FControl) and not (csDesigning in ComponentState) then
    if FControl.HandleAllocated then
      RevokeDragDrop(FControl.Handle);
end;

procedure TJvDropTarget.SetAcceptDrag(Value: Boolean);
begin
  if csLoading in ComponentState then
    FStreamedAcceptDrag := Value
  else
  if Value <> FAcceptDrag then
  begin
    UnregisterControl;
    FAcceptDrag := Value;
    RegisterControl;
  end;
end;

procedure TJvDropTarget.Loaded;
begin
  inherited Loaded;
  AcceptDrag := FStreamedAcceptDrag;
end;

procedure TJvDropTarget.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FControl) then
    Control := nil;
end;

function TJvDropTarget.GetFileDescrNames(List: TStrings): Integer;
var
  FileGroupDescr: PFileGroupDescriptor;
  Medium: TStgMedium;
  I: Integer;
  S: string;
begin
  Result := 0;
  if FDataObject.GetData(FileDescriptorFormatEtc, Medium) = S_OK then
  begin
    try
      try
        FileGroupDescr := GlobalLock(Medium.hGlobal);
        try
          if List <> nil then
          begin
            for I := 0 to FileGroupDescr.cItems - 1 do
            begin
              SetString(S, FileGroupDescr^.fgd[I].cFileName, StrLen(FileGroupDescr^.fgd[I].cFileName));
              List.Add(S);
            end;
          end;
          Result := FileGroupDescr.cItems;
        finally
          GlobalUnlock(Medium.hGlobal);
        end;
      finally
        ReleaseStgMedium(Medium);
      end;
    except
      Result := 0;
    end;
  end;
end;

function TJvDropTarget.GetFileDescrCount: Integer;
var
  FileGroupDescr: PFileGroupDescriptor;
  Medium: TStgMedium;
begin
  Result := 0;
  if FDataObject.GetData(FileDescriptorFormatEtc, Medium) = S_OK then
    try
      try
        FileGroupDescr := GlobalLock(Medium.hGlobal);
        try
          Result := FileGroupDescr.cItems;
        finally
          GlobalUnlock(Medium.hGlobal);
        end;
      finally
        ReleaseStgMedium(Medium);
      end;
    except
      Result := 0;
    end;
end;

function TJvDropTarget.GetFilenames(List: TStrings): Integer;
var
  DragH: Integer;
  Medium: TStgMedium;
  Name: string;
  I, Count, Len: Integer;
begin
  Result := 0;
  if FDataObject.GetData(FileDropFormatEtc, Medium) = S_OK then
    try
      try
        DragH := Integer(GlobalLock(Medium.hGlobal));
        try
          Count := DragQueryFile(DragH, Cardinal(-1), nil, 0);
          if List <> nil then
            for I := 0 to Count - 1 do
            begin
              Len := DragQueryFile(DragH, I, nil, 0);
              if Len > 0 then
                Inc(Len);
              SetLength(Name, Len);
              if Len > 0 then
                DragQueryFile(DragH, I, PChar(Name), Len);
              List.Add(Name);
            end;
          Result := Count;
        finally
          GlobalUnlock(Medium.hGlobal);
        end;
      finally
        ReleaseStgMedium(Medium);
      end;
    except
      Result := 0;
    end;
end;

function TJvDropTarget.GetFileContent(Index: Integer; Stream: TStream): Boolean;
const
  MaxBufSize = 100 * 1024;
var
  Medium: TStgMedium;
  InStream: IStream;
  Stat: TStatStg;

  Buf: Pointer;
  BufSize: Integer;
  Num: Int64;
  Position: Int64;
begin
  Result := False;
  if (Stream = nil) or (Index < 0) or (Index >= GetFileDescrCount) then
    Exit;

  FileContentFormatEtc.lindex := Index;
  if FDataObject.GetData(FileContentFormatEtc, Medium) = S_OK then
    try
      try
        if Medium.tymed and TYMED_ISTREAM <> 0 then
        begin
          InStream := IStream(Medium.stm);
          InStream.Stat(Stat, STATFLAG_NONAME);
          Num := Stat.cbSize;
          if Num > 0 then
          begin
            GetMem(Buf, MaxBufSize);
            try
             // Speicherbereich reservieren
              Position := Stream.Position;
              Stream.Size := Stream.Size + Num;
              Stream.Position := Position;

              while Num > 0 do
              begin
                if Num < MaxBufSize then
                  BufSize := Num
                else
                  BufSize := MaxBufSize;
                InStream.Read(Buf, BufSize, nil);
                Stream.Write(Buf^, BufSize);
                Dec(Num, BufSize);
              end;
            finally
              FreeMem(Buf);
            end;
          end;
        end
        else
          Result := False;
      finally
        ReleaseStgMedium(Medium);
      end;
    except
      Result := False;
    end;
end;

initialization
  OleInitialize(nil);

finalization
  OleUninitialize;

end.

