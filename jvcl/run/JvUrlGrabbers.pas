{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvActions.Pas, released on 2002-10-04.

The Initial Developer of the Original Code is Olivier Sannier [obones@meloo.com]
Portions created by Olivier Sannier are Copyright (C) 2003 Olivier Sannier.
All Rights Reserved.

Contributor(s): -

Last Modified: 2003-08-31

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvUrlGrabbers;

interface

uses Contnrs, Classes, JvTypes, SysUtils;

type
  // early declaration
  TJvUrlGrabberList = class;

  TJvGrabberStatus = (gsStopped, gsGrabbing);

  // The main class. It is the ancestor of all the Url Grabbers
  // and declares the required methods that a grabber must provide
  // Do not instanciate a TJvUrlGrabber directly, simply use one
  // of its descendants. This family of classes is used by
  // TJvUrlListGrabber to allow downloading a list of URLs
  TJvUrlGrabber = class (TObject)
  protected
    FOnDoneFile: TJvDoneFileEvent;
    FOnDoneStream: TJvDoneStreamEvent;
    FOnError: TJvErrorEvent;
    FOnProgress: TJvFTPProgressEvent;
    FOnClosed: TNotifyEvent;
    FOnReceiving: TNotifyEvent;
    FOnReceived: TNotifyEvent;
    FOnConnecting: TNotifyEvent;
    FOnResolving: TNotifyEvent;
    FOnRedirect: TNotifyEvent;
    FOnConnected: TNotifyEvent;
    FOnStateChange: TNotifyEvent;
    FOnResolved: TNotifyEvent;
    FOnClosing: TNotifyEvent;
    FOnRequest: TNotifyEvent;
    FOnSent: TNotifyEvent;
    FOnSending: TNotifyEvent;
    FUserName: string;
    FPassword: string;
    FFileName: TFileName;
    FOutputMode: TJvOutputMode;
    FAgent: string;
    FStatus: TJvGrabberStatus;
    FUrl: string;
  public
    constructor Create(AUrl : string); overload;

    // this function must return True if the given URL can be grabbed
    // by the class being asked. It returns False otherwise
    class function CanGrab(const Url : string) : Boolean; virtual; abstract;

    // Asks to Start to grab the URL
    procedure Start; virtual; abstract;

    // Asks to Stop to grab the URL
    procedure Stop; virtual; abstract;

    // The status of the grab
    property Status : TJvGrabberStatus read FStatus;

    // the Url being grabbed
    property Url : string read FUrl;

    // the user name and password to use for authentication 
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;

    // the name of the file to write to if OutputMode is omFile 
    property FileName: TFileName read FFileName write FFileName;

    // The output mode
    property OutputMode: TJvOutputMode read FOutputMode write FOutputMode default omStream;

    // The agent to impersonate
    property Agent: string read FAgent write FAgent;

    // Events
    property OnDoneFile           : TJvDoneFileEvent    read FOnDoneFile    write FOnDoneFile;
    property OnDoneStream         : TJvDoneStreamEvent  read FOnDoneStream  write FOnDoneStream;
    property OnError              : TJvErrorEvent       read FOnError       write FOnError;
    property OnProgress           : TJvFTPProgressEvent read FOnProgress    write FOnProgress;
    property OnResolvingName      : TNotifyEvent        read FOnResolving   write FOnResolving;
    property OnNameResolved       : TNotifyEvent        read FOnResolved    write FOnResolved;
    property OnConnectingToServer : TNotifyEvent        read FOnConnecting  write FOnConnecting;
    property OnConnectedToServer  : TNotifyEvent        read FOnConnected   write FOnConnected;
    property OnSendingRequest     : TNotifyEvent        read FOnSending     write FOnSending;
    property OnRequestSent        : TNotifyEvent        read FOnSent        write FOnSent;
    property OnRequestComplete    : TNotifyEvent        read FOnRequest     write FOnRequest;
    property OnReceivingResponse  : TNotifyEvent        read FOnReceiving   write FOnReceiving;
    property OnResponseReceived   : TNotifyEvent        read FOnReceived    write FOnReceived;
    property OnClosingConnection  : TNotifyEvent        read FOnClosing     write FOnClosing;
    property OnConnectionClosed   : TNotifyEvent        read FOnClosed      write FOnClosed;
    property OnRedirect           : TNotifyEvent        read FOnRedirect    write FOnRedirect;
    property OnStateChange        : TNotifyEvent        read FOnStateChange write FOnStateChange;
  end;

  // A grabber for FTP URLs
  TJvFtpUrlGrabber = class (TJvUrlGrabber)
  public
    class function CanGrab(const Url : string) : Boolean; override;
  end;

  // A grabber for HTTP URLs
  TJvHttpUrlGrabber = class (TJvUrlGrabber)
  public
    class function CanGrab(const Url : string) : Boolean; override;
  end;

  // A list of instances of TJvUrlGrabber descendants
  // This is used internally by TJvUrlListGrabber to keep track of
  // the objects in charge of every URLs it has to grab
  TJvUrlGrabberList = class (TObjectList)
  protected
    function GetItem(Index: Integer): TJvUrlGrabber;
    procedure SetItem(Index: Integer; const AGrabber: TJvUrlGrabber);
  public
    function Add(AGrabber : TJvUrlGrabber) : integer;
    procedure Insert(Index : integer; AGrabber : TJvUrlGrabber);

    // the items of the list
    property Items [Index : Integer] : TJvUrlGrabber read GetItem write SetItem;  default;

  end;

  TJvUrlGrabberClass = class of TJvUrlGrabber;

  // A list of classes inheriting from TJvUrlGrabber
  // This is the type of list used by the JvUrlGrabberClassList
  // function that returns all the registered classes.
  // This list is then used by TJvUrlListGrabber to determine which
  // class is best suited for handling a given URL
  TJvUrlGrabberClassList = class (TClassList)
  protected
    function GetItem(Index: Integer): TJvUrlGrabberClass;
    procedure SetItem(Index: Integer; const AGrabberClass: TJvUrlGrabberClass);
  public
    function Add(AGrabberClass : TJvUrlGrabberClass) : integer;
    procedure Insert(Index : integer; AGrabberClass : TJvUrlGrabberClass);

    // the items of the list
    property Items [Index : Integer] : TJvUrlGrabberClass read GetItem write SetItem;  default;

  end;

function JvUrlGrabberClassList : TJvUrlGrabberClassList;

implementation

// the global object to contain the list of registered
// url grabber classes
var GJvUrlGrabberClassList : TJvUrlGrabberClassList;

{ TJvUrlGrabber }

constructor TJvUrlGrabber.Create(AUrl: string);
begin
  inherited Create;
  FUrl := AUrl;
end;

{ TJvHttpUrlGrabber }

class function TJvHttpUrlGrabber.CanGrab(const Url: string): Boolean;
begin
  Result := LowerCase(Copy(Url, 1, 7)) = 'http://';
end;

{ TJvFtpUrlGrabber }

class function TJvFtpUrlGrabber.CanGrab(const Url: string): Boolean;
begin
  Result := LowerCase(Copy(Url, 1, 6)) = 'ftp://';
end;

{ TJvUrlGrabberList }

function TJvUrlGrabberList.Add(AGrabber: TJvUrlGrabber): integer;
begin
  Result := inherited Add(AGrabber);
end;

function TJvUrlGrabberList.GetItem(Index: Integer): TJvUrlGrabber;
begin
  Result := TJvUrlGrabber(inherited Items[Index]);
end;

procedure TJvUrlGrabberList.Insert(Index: integer;
  AGrabber: TJvUrlGrabber);
begin
  inherited Insert(Index, AGrabber);
end;

procedure TJvUrlGrabberList.setItem(Index: Integer;
  const AGrabber: TJvUrlGrabber);
begin
  inherited Items[Index] := AGrabber;
end;

{ TJvUrlGrabberClassList }

function TJvUrlGrabberClassList.Add(AGrabberClass: TJvUrlGrabberClass): integer;
begin
  Result := inherited Add(AGrabberClass);
end;

function TJvUrlGrabberClassList.GetItem(
  Index: Integer): TJvUrlGrabberClass;
begin
  Result := TJvUrlGrabberClass(inherited Items[Index]);
end;

procedure TJvUrlGrabberClassList.Insert(Index: integer;
  AGrabberClass: TJvUrlGrabberClass);
begin
  inherited Insert(Index, AGrabberClass);
end;

procedure TJvUrlGrabberClassList.SetItem(Index: Integer;
  const AGrabberClass: TJvUrlGrabberClass);
begin
  inherited Items[Index] := AGrabberClass;
end;

function JvUrlGrabberClassList : TJvUrlGrabberClassList;
begin
  Result := GJvUrlGrabberClassList;
end;

initialization
  // create the object
  GJvUrlGrabberClassList := TJvUrlGrabberClassList.Create;

  // register the classes
  GJvUrlGrabberClassList.Add(TJvFtpUrlGrabber);
  GJvUrlGrabberClassList.Add(TJvHttpUrlGrabber);

finalization
  GJvUrlGrabberClassList.Free;

end.
