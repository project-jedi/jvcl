unit SimpleFormU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TFormEvent = (feOnClose, feOnCreate, feOnDestroy, feOnShow);

  TMsgEvent = procedure(Sender: TObject; const Msg: TMessage) of object;
  TEventEvent = procedure(Sender: TObject; const Event: TFormEvent) of object;

  TSimpleFrm = class(TForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  protected
    procedure SendMessage(const Msg: TMessage);
    procedure SendEvent(const Event: TFormEvent);
  public
    class procedure RegisterClient(Client: TObject; MsgEvent: TMsgEvent = nil; EventEvent: TEventEvent = nil); virtual;
    class procedure UnRegisterClient(Client: TObject); virtual;
    class procedure Execute;
    class procedure Final;
    class function Instance: TSimpleFrm;
    class function InstanceAllocated: Boolean;
  end;

implementation

{$R *.dfm}

var
  GInstance: TSimpleFrm = nil;
  GClients: TList;
  GMsgEvents: TList;
  GEventEvents: TList;

{ TSimpleFrm }

class procedure TSimpleFrm.Execute;
begin
  Instance.Show;
end;

class procedure TSimpleFrm.Final;
begin
  GInstance.Free;
  GInstance := nil;
end;

class function TSimpleFrm.Instance: TSimpleFrm;
begin
  if not Assigned(GInstance) then
    GInstance := TSimpleFrm.Create(nil);

  Result := GInstance;
end;

procedure TSimpleFrm.FormDestroy(Sender: TObject);
begin
  SendEvent(feOnDestroy);
  GInstance := nil;
end;

class procedure TSimpleFrm.RegisterClient(Client: TObject;
  MsgEvent: TMsgEvent; EventEvent: TEventEvent);
begin
  if Assigned(GClients) then
  begin
    GClients.Add(Client);
    GMsgEvents.Add(TMethod(MsgEvent).Code);
    GEventEvents.Add(TMethod(EventEvent).Code);
  end;
end;

class procedure TSimpleFrm.UnRegisterClient(Client: TObject);
var
  Index: Integer;
begin
  if Assigned(GClients) then
  begin
    Index := GClients.IndexOf(Client);
    if Index <> -1 then
    begin
      GClients.Delete(Index);
      GMsgEvents.Delete(Index);
      GEventEvents.Delete(Index);
    end;
  end;
end;

procedure TSimpleFrm.SendEvent(const Event: TFormEvent);
var
  I: Integer;
  EventEvent: TEventEvent;
begin
  for I := 0 to GClients.Count - 1 do
    if GEventEvents[I] <> nil then
    begin
      TMethod(EventEvent).Code := GEventEvents[I];
      TMethod(EventEvent).Data := GClients[I];
      EventEvent(Self, Event);
    end;
end;

procedure TSimpleFrm.SendMessage(const Msg: TMessage);
var
  I: Integer;
  MsgEvent: TMsgEvent;
begin
  for I := 0 to GClients.Count - 1 do
    if GMsgEvents[I] <> nil then
    begin
      TMethod(MsgEvent).Code := GMsgEvents[I];
      TMethod(MsgEvent).Data := GClients[I];
      MsgEvent(Self, Msg);
    end;
end;

procedure TSimpleFrm.FormShow(Sender: TObject);
begin
  SendEvent(feOnShow);
end;

procedure TSimpleFrm.FormCreate(Sender: TObject);
begin
  SendEvent(feOnCreate);
end;

procedure TSimpleFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SendEvent(feOnClose);
end;

class function TSimpleFrm.InstanceAllocated: Boolean;
begin
  Result := Assigned(GInstance);
end;

initialization
  GClients := TList.Create;
  GMsgEvents := TList.Create;
  GEventEvents := TList.Create;
finalization
  TSimpleFrm.Final;
  FreeAndNil(GClients);
  FreeAndNil(GMsgEvents);
  FreeAndNil(GEventEvents);
end.

