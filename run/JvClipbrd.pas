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

Last Modified: 2003-05-20

You may retrieve the latest version of this file at the Connection Manager
home page, located at http://cnxmanager.sourceforge.net

Known Issues: none to date.
-----------------------------------------------------------------------------}
unit JvClipbrd;

interface

uses Clipbrd, Messages, Classes, Contnrs;

// the string displayed when an exception is risen because no
// OnRenderFormat handler was given. Put in resources to allow to
// be translated
resourcestring SNoRenderFormatEventGiven = 'No OnRenderFormat was given.';

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
  //              mustFree parameter to true to ask the clipboard to
  //              free the memory for you (done using FreeMem)
  //              Setting buffer to nil will make the system remove this
  //              format from the clipboard, even if it is not a
  //              documented behaviour
  //   size       the size in bytes of the data available in the buffer
  //              Setting size to 0 will make the system remove this
  //              format from the clipboard, even if it is not a
  //              documented behaviour
  //   mustFree   set to false by default. If you set this to true, the
  //              clipboard will call FreeMem(buffer) after having copied
  //              the data in the system clipboard. This is useful only
  //              if you allocated the buffer using GetMem()
  TOnRenderFormat = procedure (Sender : TObject;
                               Format : Word;
                               var buffer : pointer;
                               var size : cardinal;
                               var mustFree : boolean)
                    of object;

  // the new clipboard object, with added power !
  // clearly, it now allows to use delayed rendering through the
  // OnRenderFormat event.
  // All methods have been overriden to allow specifying delayed
  // rendering. See their documentation for details.
  // However, SetAsHandle has not been overriden as it is enough
  // to set its second parameter to 0 to get delayed rendering
  TJvClipboard = class (TClipboard)
  private
  protected
    // the pointer to the user procedure to call for the event
    // see the declaration of TOnRenderFormat
    EOnRenderFormat : TOnRenderFormat;

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
    delayedFormats : TList;

    // the handle to the window processing the messages
    FClipboardWindow : THandle;

    // This flag is used to determine wether or not the RenderFormat
    // method should be called as a result of the WM_RENDERALLFORMATS
    // message. It will be set to true from within DestroyHandle,
    // thus ensuring a delayed rendering format is only rendered once
    FFromDestroyHandle : boolean;

    // overridden wndproc to handle WM_RENDERFORMAT and
    // WM_RENDERALLFORMATS messages
    procedure WndProc(var Message: TMessage); override;

    // This function calls the user event handler and does the
    // rendering the way windows expects it
    // it will trigger an exception if no OnRenderFormat event
    // handler is given
    procedure RenderFormat(Format : Word); virtual;

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
    
    // forced to override Open to be able to use our own
    // window handle
    procedure Open; override;

    // Close is overriden but simply calls the inherited Close
    // method. It is still there if you want to tweak around
    procedure Close; override;

    // forced to override Clear to keep track of the delayed
    // formats in the delayedFormats list
    procedure Clear; override;

    // registers a format of that name with the system and returns
    // its identifier. You may as well call RegisterClipboardFormat
    // directly
    function RegisterFormat(name : string) : Word;

    // add a format that uses delayed rendering
    // if you do so, you MUST provide an OnRenderFormat event handler
    procedure AddDelayed(format : word);

    // overriden method to allow setting buffer to nil and thus
    // asking to use delayed rendering. If you do so, you MUST provide
    // an OnRenderFormat event handler
    // if buffer <> nil then the inherited method is called
    procedure SetBuffer(Format: Word; Buffer : pointer; Size: Integer); overload;

    // get a buffer of the given format
    // the format must be present in the clipboard. If not the function
    // returns false.
    // The buffer and the size parameters must be set to the correct size
    // for the specified format. If they are too small, the data will be
    // truncated, resulting in corrupted values on your side (but you're
    // the one who knows what to do with that).
    // If they are too large, the application will crash as it will be
    // asking the system for more data than available
    // Returns true if data was successfuly retrieved, else use
    // Windows.GetLastError to get the error code
    function GetBuffer(format : word; buffer : pointer; size : cardinal) : boolean;

    // overloaded version of the same procedure in TClipboard that
    // now allows you to specify delayed rendering.
    // If delayed is set to false, the inherited method is called
    // else, the format is simply added in the clipboard and you MUST
    // provid an OnRenderFormat event;
    procedure SetComponent(Component: TComponent; delayed : boolean); overload;

    // overloaded version of the same procedure in TClipboard that
    // now allows you to specify delayed rendering.
    // If delayed is set to false, the inherited method is called
    // else, the format is simply added in the clipboard and you MUST
    // provid an OnRenderFormat event;
    procedure SetTextBuf(Buffer: PChar; delayed : boolean); overload;

    // the handle to the underlying window handling the delayed
    // rendering messages
    property Handle : THandle read GetHandle;
  published
    // the event fired when a format has been added with delayed
    // rendering and needs rendering because an application (or the
    // DestroyHandle method) asked for it
    property OnRenderFormat : TOnRenderFormat read EOnRenderFormat write EOnRenderFormat;
  end;

// global function to get access to a TJvClipboard object
function JvClipboard: TJvClipboard;

implementation

uses Windows, SysUtils, Consts;

{ TJvClipboard }

procedure TJvClipboard.Clear;
begin
  // call the inherited method, will do its job
  inherited;
  // no more delayed formats available
  delayedFormats.Clear;
end;

function TJvClipboard.GetBuffer(format: word; buffer: pointer;
  size: cardinal) : boolean;
var Data : THandle;
    DataPtr : pointer;
begin
  // retrieve data of the given format

  // first, open clipoard
  Open;

  // ask for data
  Data := GetClipboardData(format);

  // was data retrieved ?
  if Data <> 0 then
  begin
    // if some data retrieved, get a pointer to it
    DataPtr := GlobalLock(Data);

    // did we get a valid pointer ?
    if DataPtr <> nil then
    begin
      // if yes, copy from global pointer to user supplied pointer
      CopyMemory(buffer, DataPtr, size);
      // and retrieval was a success
      Result := true;
    end
    else
    begin
      // else, retrieval has failed
      Result := false;
    end;

    // unlock global memory
    GlobalUnlock(Data);
  end
  else
  begin
    // if no data retrieved, then retrieval failed
    Result := false;
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
      FClipboardWindow := Classes.AllocateHWnd(MainWndProc);
    end;

    // we must now close the clipboard as it was opened
    // with an incorrect window handle (most likely the
    // application window handle)
    CloseClipboard;

    // and we finally open the clipboard with our window handle
    // to ensure that we can process delayed rendering messages
    if not OpenClipboard(FClipboardWindow) then
      raise Exception.CreateRes(@SCannotOpenClipboard);
  end;
end;

procedure TJvClipboard.Close;
begin
  // call the inherited close method to force update of the
  // inherited FOpenRefCount and to close the clipboard if
  // needed
  inherited Close;
end;

function TJvClipboard.RegisterFormat(name: string) : Word;
var tmp : PChar;
begin
  GetMem(tmp, length(name)+1); // don't forget +1 for trailing #0
  StrPCopy(tmp, name);
  Result := RegisterClipboardFormat(tmp);
  FreeMem(tmp);
  // Note : Yes, we could have used PChar(name) as an argument to
  // RegisterClipboardFormat, but this only works under Delphi 6
  // and this code have work under older versions
end;

procedure TJvClipboard.RenderFormat(Format: Word);
var buffer: pointer;
    size: cardinal;
    hglb : HGLOBAL;
    globalPtr : pointer;
    mustFree : boolean;
begin
  // by default, we must not free the given buffer
  mustFree := false;

  // if user gave us an event
  if Assigned(EOnRenderFormat) then
  begin
    // then ask user to render the format
    EOnRenderFormat(Self, Format, buffer, size, mustFree);
  end
  else
  begin
    // else, trigger an exception, how could we guess the
    // size and data to put in the buffer ?
    raise Exception.CreateRes(@SNoRenderFormatEventGiven);
  end;

  // now render the way windows wants it

  // first allocate a global memory
  hglb := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, size);
  if hglb <> 0 then
  begin
    // if allocation was successful
    // then lock global memory to get access to it
    globalPtr := GlobalLock(hglb);

    // copy user supplied data
    CopyMemory(globalPtr, buffer, size);

    // unlock global memory
    GlobalUnlock(hglb);

    // finally, place the content in the clipboard
    SetClipboardData(format, hglb);
  end;

  // if user asked us to free his buffer
  if mustFree then
  begin
    // then we free it
    FreeMem(buffer);
  end;
end;

procedure TJvClipboard.WndProc(var Message: TMessage);
var i : integer;
begin
  case message.Msg of
    // if asked to render a particular format
    WM_RENDERFORMAT :     begin
                            // then render it
                            RenderFormat(message.WParam);
                            // and tell windows so
                            message.Result := 0;
                          end;
    // if asked to render all available formats
    WM_RENDERALLFORMATS : begin
                            // then if it is not the result of a call
                            // to DestroyHandle
                            if not FFromDestroyHandle then
                            begin
                              // then we render all the delayed formats
                              // we are aware of
                              for i := 0 to delayedFormats.Count-1 do
                              begin
                                RenderFormat(Word(delayedFormats[i]));
                              end;
                            end;
                            // tell windows we handled the message
                            message.Result := 0;
                          end;
  end;

  // in any case let the ancestor do its stuff
  inherited;
end;

constructor TJvClipboard.Create;
begin
  inherited;
  // create the list used to keep track of delayed formats
  // in the clipboard
  delayedFormats := TList.Create;

  // if a WM_RENDERALLFORMATS message is fired, then
  // it is not yet as a result of a call to DestroyHandle 
  FFromDestroyHandle := false;
end;

destructor TJvClipboard.Destroy;
begin
  // ensure handle is destroyed, but see remark where
  // DestroyHandle is declared
  DestroyHandle;

  // free the list
  delayedFormats.Free;

  // and let the rest be done
  inherited;
end;

procedure TJvClipboard.DestroyHandle;
var i : integer;
    format : word;
    buffer : char;
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
    for i := 0 to delayedFormats.Count - 1 do
    begin
      // get the format id
      format := Word(delayedFormats[i]);

      // ask to get this format from the clipboard, will
      // in turn trigger a WM_RENDERFORMAT message
      // we only ask for one byte as we don't know what to
      // do with the format and clearly won't use it
      // Asking for one byte ensures that windows will
      // effectively give us something
      GetBuffer(format, @buffer, 1);
    end;

    // Having done that will not prevent the WM_RENDERALLFORMATS
    // message from being fired so me must ensure the RenderFormat
    // method is not called twice for all delayed rendering formats
    FFromDestroyHandle := true;
    
    // we can now safely destroy the window
    Classes.DeallocateHWnd(FClipboardWindow);

    // and we no longer have a window
    FClipboardWindow := 0;
  end;
end;

function TJvClipboard.GetHandle: THandle;
begin
  Result := FClipboardWindow;
end;

procedure TJvClipboard.SetComponent(Component: TComponent;
  delayed: boolean);
begin
  if delayed then
  begin
    // add as delayed
    AddDelayed(CF_COMPONENT);
  end
  else
  begin
    inherited SetComponent(Component);
  end;
end;

procedure TJvClipboard.SetTextBuf(Buffer: PChar; delayed: boolean);
begin
  if delayed then
  begin
    // add as delayed
    AddDelayed(CF_TEXT);
  end
  else
  begin
    inherited SetTextBuf(Buffer);
  end;
end;

procedure TJvClipboard.SetBuffer(Format: Word; Buffer: pointer;
  Size: Integer);
begin
  // if buffer is nil
  if Buffer = nil then
  begin
    // then add the format using delayed rendering
    AddDelayed(Format);
  end
  else
  begin
    // else call inherited method
    inherited SetBuffer(Format, Buffer, size);
  end;

end;

procedure TJvClipboard.AddDelayed(format: word);
begin
  // add as delayed
  inherited SetAsHandle(Format, 0);
  // and we keep track of that format
  delayedFormats.Add(Pointer(Format));
end;

var
  // global variable
  FJvClipboard: TJvClipboard;

// global function to call to get access to the clipboard
function JvClipboard: TJvClipboard;
begin
  if FJvClipboard = nil then
    FJvClipboard := TJvClipboard.Create;
  Result := FJvClipboard;
end;

initialization

finalization
  FJvClipboard.Free;

end.
