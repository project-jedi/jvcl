{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvClipbrd.PAS, released on 2003-05-18.

The Initial Developer of the Original Code is Olivier Sannier
[obones@meloo.com]
Portions created by Olivier Sannier are Copyright (C) 2003 Olivier Sannier.
All Rights Reserved.

Contributor(s): none to date.

You may retrieve the latest version of this file at the Connection Manager
home page, located at http://cnxmanager.sourceforge.net

Known Issues: none to date.
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvClipbrd;

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Clipbrd,
  JvFinalize;

type
  // the type of the event fired when a format has been added with delayed
  // rendering and needs rendering because an application asked for it.
  // Parameters are :
  //   Sender     The object triggering the event. Cleary, will always
  //              be the value returned by JvClipboard, unless you
  //              call the event yourself
  //   Format     The format needing rendering
  //   buffer     The buffer where the rendered data has to be or
  //              has been put
  //              You may change the value of the buffer parameter to
  //              point on a memory that will survive the exit of your
  //              handler. Clearly, you can't give a pointer to a local
  //              variable or use a PChar() conversion.
  //              Alternatively, you can allocate some memory with
  //              GetMem(), copy your data in the buffer and set the
  //              mustFree parameter to True to ask the clipboard to
  //              free the memory for you (done using FreeMem)
  //              Setting buffer to nil will make the system remove this
  //              format from the clipboard, even if it is not a
  //              documented behaviour
  //   size       the size in bytes of the data available in the buffer
  //              Setting size to 0 will make the system remove this
  //              format from the clipboard, even if it is not a
  //              documented behaviour
  //   mustFree   set to False by default. If you set this to True, the
  //              clipboard will call FreeMem(buffer) after having copied
  //              the data in the system clipboard. This is useful only
  //              if you allocated the buffer using GetMem()
  TJvRenderFormatEvent = procedure(Sender: TObject; Format: Word;
    var Buffer: Pointer; var Size: Cardinal; var MustFree: Boolean) of object;

  // the new clipboard object, with added power !
  // clearly, it now allows to use delayed rendering through the
  // OnRenderFormat event.
  // All methods have been overriden to allow specifying delayed
  // rendering. See their documentation for details.
  // However, SetAsHandle has not been overriden as it is enough
  // to set its second parameter to 0 to get delayed rendering
  TJvClipboard = class(TClipboard)
  private
    function GetAsWideText: WideString;
    procedure SetAsWideText(const Value: WideString);
  protected
    // the pointer to the user procedure to call for the event
    // see the declaration of TOnRenderFormat
    FOnRenderFormat: TJvRenderFormatEvent;

    // the list of formats that have been added using delayed rendering
    // we need to keep track of them because we must be able to render
    // them when the WM_RENDERALLFORMATS event is fired.
    // And we can't simply loop through the Formats property as this
    // is not a good thing to call RenderFormat for formats not put
    // by the user in the clipboard (for instance, adding CF_TEXT will
    // make the system add a CF_OEMTEXT and a CF_UNICODETEXT automatically)
    // Moreover, to ensure that the delayed rendering formats will
    // survive the death of the application, we must ask for their value
    // from the DestroyHandle method
    // If the formats are quite big, which may cause memory problems,
    // you should ask the user if he wants to keep them and if not,
    // call the clear method before destroying the handle
    // This list will actually only contain Words (Format Ids)
    FDelayedFormats: TList;

    // the handle to the window processing the messages
    FClipboardWindow: THandle;

    // This flag is used to determine wether or not the RenderFormat
    // method should be called as a result of the WM_RENDERALLFORMATS
    // message. It will be set to True from within DestroyHandle,
    // thus ensuring a delayed rendering format is only rendered once
    FFromDestroyHandle: Boolean;

    // overridden wndproc to handle WM_RENDERFORMAT and
    // WM_RENDERALLFORMATS messages
    {$IFDEF COMPILER6_UP}
    procedure WndProc(var Message: TMessage); override;
    {$ELSE}
    procedure WndProc(var Message: TMessage); virtual;
    procedure MainWndProc(var Message: TMessage);
    procedure SetBuffer(Format: Word; var Buffer; Size: Integer); overload;
    {$ENDIF COMPILER6_UP}

    // This function calls the user event handler and does the
    // rendering the way windows expects it
    // it will trigger an exception if no OnRenderFormat event
    // handler is given
    procedure RenderFormat(Format: Word); virtual;

    // returns the window handle (the value of FClipboardWindow)
    function GetHandle: THandle;
  public
    // creates the list
    constructor Create; virtual;

    // destroys the list and the window (only if needed, see below)
    destructor Destroy; override;

    // To ensure that the formats using delayed rendering are
    // saved when the application terminates, it is necessary
    // that we get their values before actually destroying the
    // underlying window (which handle is given by the Handle property)
    // As a result, the OnRenderFormat event will be fired for every
    // format with delayed rendering still in the clipboard.
    // This could lead to memory problems if the format is quite big.
    // You should then asks the user if he wants to keep the big objects
    // available for other programs
    // If you don't call this method and some formats are in the
    // clipboard with delayed rendering, it will be called upon
    // destruction of the clipboard, which is likely to happen after the
    // destruction of the object where the event is set.
    // I let you imagine the consequences...
    // So you should call this function from the destructor (or the
    // OnDestroy event) of the component where the OnRenderFormat event
    // handler is set (eg, the main form) as this component is likely
    // to be destroyed before the clipboard itself.
    procedure DestroyHandle;

    {$IFDEF COMPILER6_UP}
    // forced to override Open to be able to use our own
    // window handle
    procedure Open; override;

    // Close is overriden but simply calls the inherited Close
    // method. It is still there if you want to tweak around
    procedure Close; override;

    // forced to override Clear to keep track of the delayed
    // formats in the delayedFormats list
    procedure Clear; override;
    {$ELSE}
    procedure Open; virtual;
    procedure Close; virtual;
    procedure Clear; virtual;
    {$ENDIF COMPILER6_UP}

    // registers a format of that name with the system and returns
    // its identifier. You may as well call RegisterClipboardFormat
    // directly
    function RegisterFormat(const Name: string): Word;

    // add a format that uses delayed rendering
    // if you do so, you MUST provide an OnRenderFormat event handler
    procedure AddDelayed(Format: Word);

    // overriden method to allow setting buffer to nil and thus
    // asking to use delayed rendering. If you do so, you MUST provide
    // an OnRenderFormat event handler
    // if buffer <> nil then the inherited method is called
    procedure SetBuffer(Format: Word; Buffer: Pointer; Size: Integer); overload;

    // get a buffer of the given format
    // the format must be present in the clipboard. If not the function
    // returns False.
    // The buffer and the size parameters must be set to the correct size
    // for the specified format. If they are too small, the data will be
    // truncated, resulting in corrupted values on your side (but you're
    // the one who knows what to do with that).
    // If they are too large, the application will crash as it will be
    // asking the system for more data than available
    // Returns True if data was successfuly retrieved, else use
    // Windows.GetLastError to get the error code
    function GetBuffer(Format: Word; Buffer: Pointer; Size: Cardinal): Boolean;

    // overloaded version of the same procedure in TClipboard that
    // now allows you to specify delayed rendering.
    // If delayed is set to False, the inherited method is called
    // else, the format is simply added in the clipboard and you MUST
    // provid an OnRenderFormat event;
    procedure SetComponent(Component: TComponent; Delayed: Boolean); overload;

    // overloaded version of the same procedure in TClipboard that
    // now allows you to specify delayed rendering.
    // If delayed is set to False, the inherited method is called
    // else, the format is simply added in the clipboard and you MUST
    // provid an OnRenderFormat event;
    procedure SetTextBuf(Buffer: PChar; Delayed: Boolean); overload;

    // the handle to the underlying window handling the delayed
    // rendering messages
    property Handle: THandle read GetHandle;
  {$IFNDEF COMPILER6_UP}
  private
    function GetOpenRefCount: Integer;
  protected
    property OpenRefCount: Integer read GetOpenRefCount;
  {$ENDIF !COMPILER6_UP}
  published
    // the event fired when a format has been added with delayed
    // rendering and needs rendering because an application (or the
    // DestroyHandle method) asked for it
    property OnRenderFormat: TJvRenderFormatEvent read FOnRenderFormat
      write FOnRenderFormat;
    property AsWideText: WideString read GetAsWideText write SetAsWideText;
  end;

// global function to get access to a TJvClipboard object
function JvClipboard: TJvClipboard;

implementation

uses
  Consts,
  JvTypes, JvJVCLUtils, JvResources;

const
  sUnitName = 'JvClipbrd';

{$IFNDEF COMPILER6_UP}

// Delphi 5 implementation
type
  TPrivateClipboard = class(TPersistent)
  private
    FOpenRefCount: Integer;
  end;

var
  Clipboard_SetBuffer: procedure(Instance: TClipboard; Format: Word; var Buffer;
    Size: Integer);

{$ENDIF !COMPILER6_UP}

constructor TJvClipboard.Create;
begin
  inherited Create;
  // create the list used to keep track of delayed formats
  // in the clipboard
  FDelayedFormats := TList.Create;

  // if a WM_RENDERALLFORMATS message is fired, then
  // it is not yet as a result of a call to DestroyHandle
  FFromDestroyHandle := False;
end;

destructor TJvClipboard.Destroy;
begin
  // ensure handle is destroyed, but see remark where
  // DestroyHandle is declared
  DestroyHandle;

  // free the list
  FDelayedFormats.Free;

  // and let the rest be done
  inherited Destroy;
end;

{$IFNDEF COMPILER6_UP}

procedure TJvClipboard.SetBuffer(Format: Word; var Buffer; Size: Integer);
var
  P: PByte;
begin
  if not Assigned(Clipboard_SetBuffer) then
  begin
    P := @TClipboard.SetTextBuf;
    while P^ <> $E8 do
      Inc(P); // StrLen
    Inc(P);
    while P^ <> $E8 do
      Inc(P); // SetBuffer
    Inc(P);
    Clipboard_SetBuffer := Pointer(Integer(P) + 4 + PInteger(P)^);
  end;
  if Assigned(Clipboard_SetBuffer) then
    Clipboard_SetBuffer(Self, Format, Buffer, Size);
end;

function TJvClipboard.GetOpenRefCount: Integer;
begin
  Result := TPrivateClipboard(Self).FOpenRefCount;
end;

{$ENDIF !COMPILER6_UP}

procedure TJvClipboard.Clear;
begin
  // call the inherited method, will do its job
  inherited Clear;
  // no more delayed formats available
  FDelayedFormats.Clear;
end;

function TJvClipboard.GetBuffer(Format: Word; Buffer: Pointer;
  Size: Cardinal): Boolean;
var
  Data: THandle;
  DataPtr: Pointer;
begin
  // retrieve data of the given format

  // first, open clipoard
  Open;

  // ask for data
  Data := GetClipboardData(Format);

  // was data retrieved ?
  if Data <> 0 then
  begin
    // if some data retrieved, get a Pointer to it
    DataPtr := GlobalLock(Data);

    // did we get a valid Pointer ?
    if DataPtr <> nil then
    begin
      // if yes, copy from global pointer to user supplied pointer
      CopyMemory(buffer, DataPtr, Size);
      // and retrieval was a success
      Result := True;
    end
    else
    begin
      // else, retrieval has failed
      Result := False;
    end;

    // unlock global memory
    GlobalUnlock(Data);
  end
  else
  begin
    // if no data retrieved, then retrieval failed
    Result := False;
  end;

  // finally, close clipoard
  Close;
end;

procedure TJvClipboard.Open;
begin
  // call the inherited open method to force the inherited
  // private FOpenRefCount to be greater than 0. This is the
  // result of a bad design, because FOpenRefCount should be
  // protected in the TClipboard class, allowing us to access
  // it directly, rather than tweaking around
  // Having the inherited FOpenRefCount greater than 0 is
  // required for the inherited method that put data in the
  // clipboard to work. Indeed, they call the private method
  // Adding which calls Clear only if FOpenRefCount is not 0.
  // And calling Clear is required for the window to get the
  // clipboard ownership.
  // Another good decision would have been to make the Adding
  // method protected rather than private. This would have
  // allowed to easily add other methods to put other types
  // in the clipboard.
  // But it seems the people in charge of that part didn't
  // have reusability in mind when they designed the
  // TClipboard class
  inherited Open;

  // if we were just opened (the inherited FOpenRefCount
  // just turned to 1)
  if OpenRefCount = 1 then
  begin
    // then, if we need a window to handle delayed rendering
    if FClipboardWindow = 0 then
    begin
      // then we create one, passing MainWndProc rather than
      // WndProc as MainWndProc will call WndProc but in a
      // try except statement ensuring good exception handling
      FClipboardWindow := AllocateHWndEx(MainWndProc);
    end;

    // we must now close the clipboard as it was opened
    // with an incorrect window handle (most likely the
    // application window handle)
    CloseClipboard;

    // and we finally open the clipboard with our window handle
    // to ensure that we can process delayed rendering messages
    if not OpenClipboard(FClipboardWindow) then
      raise EJVCLException.Create(SCannotOpenClipboard);
  end;
end;

procedure TJvClipboard.Close;
begin
  // call the inherited close method to force update of the
  // inherited FOpenRefCount and to close the clipboard if
  // needed
  inherited Close;
end;

function TJvClipboard.RegisterFormat(const Name: string): Word;
var
  Tmp: PChar;
begin
  GetMem(Tmp, Length(Name) + 1); // don't forget +1 for trailing #0
  try
    StrPCopy(Tmp, Name);
    Result := RegisterClipboardFormat(Tmp);
  finally
    FreeMem(Tmp);
  end;
  // Note : Yes, we could have used PChar(name) as an argument to
  // RegisterClipboardFormat, but this only works under Delphi 6
  // and this code have to work under older versions
end;

procedure TJvClipboard.RenderFormat(Format: Word);
var
  Buffer: Pointer;
  Size: Cardinal;
  hglb: HGLOBAL;
  GlobalPtr: Pointer;
  MustFree: Boolean;
begin
  // by default, we must not free the given buffer
  MustFree := False;

  // if user gave us an event
  if Assigned(FOnRenderFormat) then
    // then ask user to render the format
    FOnRenderFormat(Self, Format, Buffer, Size, MustFree)
  else
    // else, trigger an exception, how could we guess the
    // size and data to put in the buffer ?
    raise EJVCLException.Create(RsENoRenderFormatEventGiven);

  // now render the way windows wants it

  // first allocate a global memory
  hglb := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, Size);
  if hglb <> 0 then
  begin
    // if allocation was successful
    // then lock global memory to get access to it
    globalPtr := GlobalLock(hglb);

    // copy user supplied data
    CopyMemory(GlobalPtr, Buffer, Size);

    // unlock global memory
    GlobalUnlock(hglb);

    // finally, place the content in the clipboard
    SetClipboardData(Format, hglb);
  end;

  // if user asked us to free his buffer
  if MustFree then
    // then we free it
    FreeMem(buffer);
end;

procedure TJvClipboard.WndProc(var Message: TMessage);
var
  I: Integer;
begin
  case Message.Msg of
    // if asked to render a particular format
    WM_RENDERFORMAT:
      begin
        // then render it
        RenderFormat(Message.WParam);
        // and tell windows so
        Message.Result := 0;
      end;
    // if asked to render all available formats
    WM_RENDERALLFORMATS:
      begin
        // then if it is not the result of a call
        // to DestroyHandle
        if not FFromDestroyHandle then
        begin
          // then we render all the delayed formats
          // we are aware of
          for I := 0 to FDelayedFormats.Count - 1 do
            RenderFormat(Word(FDelayedFormats[I]));
        end;
        // tell windows we handled the message
        Message.Result := 0;
      end;
  end;

  {$IFDEF COMPILER6_UP}
  // in any case let the ancestor do its stuff
  inherited WndProc(Message);
  {$ELSE}
  with Message do
    Result := DefWindowProc(Handle, Msg, WParam, LParam);
  {$ENDIF COMPILER6_UP}
end;

{$IFNDEF COMPILER6_UP}
procedure TJvClipboard.MainWndProc(var Message: TMessage);
begin
  try
    WndProc(Message);
  except
    ShowException(ExceptObject, ExceptAddr);
  end;
end;
{$ENDIF !COMPILER6_UP}

procedure TJvClipboard.DestroyHandle;
var
  I: Integer;
  Format: Word;
  Buffer: Char;
begin
  // if we have a window handle, hence, meaning that it is
  // the first time DestroyHandle is called
  if FClipboardWindow <> 0 then
  begin
    // to ensure persistance of the private formats, we
    // must get them before destroying the window
    // this is rather strange as destroying the window fires
    // the WM_RENDERALLFORMATS message but it seems the system
    // forgets the results.
    // so we do the job ourselves and ask for the data
    // Of course, this will not work for formats that the user
    // put in the clipboard using delayed rendering through direct
    // API calls
    for I := 0 to FDelayedFormats.Count - 1 do
    begin
      // get the format id
      Format := Word(FDelayedFormats[I]);

      // ask to get this format from the clipboard, will
      // in turn trigger a WM_RENDERFORMAT message
      // we only ask for one byte as we don't know what to
      // do with the format and clearly won't use it
      // Asking for one byte ensures that windows will
      // effectively give us something
      GetBuffer(Format, @Buffer, 1);
    end;

    // Having done that will not prevent the WM_RENDERALLFORMATS
    // message from being fired so me must ensure the RenderFormat
    // method is not called twice for all delayed rendering formats
    FFromDestroyHandle := True;

    // we can now safely destroy the window
    DeallocateHWndEx(FClipboardWindow);

    // and we no longer have a window
    FClipboardWindow := 0;
  end;
end;

function TJvClipboard.GetHandle: THandle;
begin
  Result := FClipboardWindow;
end;

procedure TJvClipboard.SetComponent(Component: TComponent; Delayed: Boolean);
begin
  if Delayed then
    // add as delayed
    AddDelayed(CF_COMPONENT)
  else
    inherited SetComponent(Component);
end;

procedure TJvClipboard.SetTextBuf(Buffer: PChar; Delayed: Boolean);
begin
  if Delayed then
    // add as delayed
    AddDelayed(CF_TEXT)
  else
    inherited SetTextBuf(Buffer);
end;

procedure TJvClipboard.SetBuffer(Format: Word; Buffer: Pointer; Size: Integer);
begin
  // if buffer is nil
  if Buffer = nil then
    // then add the format using delayed rendering
    AddDelayed(Format)
  else
  begin
    // else call inherited method
    {$IFDEF COMPILER6_UP}
    inherited SetBuffer(Format, Buffer^, Size);
    {$ELSE}
    SetBuffer(Format, Buffer^, Size);
    {$ENDIF COMPILER6_UP}
  end;
end;

procedure TJvClipboard.AddDelayed(Format: Word);
begin
  // add as delayed
  inherited SetAsHandle(Format, 0);
  // and we keep track of that format
  FDelayedFormats.Add(Pointer(Format));
end;

function TJvClipboard.GetAsWideText: WideString;
var
  Data: THandle;
begin
  Open;
  Data := GetClipboardData(CF_UNICODETEXT);
  try
    if Data <> 0 then
      Result := PWideChar(GlobalLock(Data))
    else
      Result := '';
  finally
    if Data <> 0 then
      GlobalUnlock(Data);
    Close;
  end;
  if (Data = 0) or (Result = '') then
    Result := AsText
end;

procedure TJvClipboard.SetAsWideText(const Value: WideString);
begin
  Open;
  try
    AsText := Value; {Ensures ANSI compatiblity across platforms.}
    SetBuffer(CF_UNICODETEXT, PWideChar(Value)^, (Length(Value) + 1) * SizeOf(WideChar));
  finally
    Close;
  end;
end;

var
  GlobalClipboard: TJvClipboard;

// global function to call to get access to the clipboard

function JvClipboard: TJvClipboard;
begin
  if GlobalClipboard = nil then
  begin
    GlobalClipboard := TJvClipboard.Create;
    AddFinalizeObjectNil(sUnitName, TObject(GlobalClipboard));
  end;
  Result := GlobalClipboard;
end;

initialization

finalization
  FinalizeUnit(sUnitName);

end.

