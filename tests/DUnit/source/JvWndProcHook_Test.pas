unit JvWndProcHook_Test;

interface

uses
  Windows, Controls, Messages, SysUtils, Forms,
  TestFrameWork, JvWndProcHook,
  SimpleFormU;

const
  WM_TEST = WM_APP + 0;
  WM_SENDINHANDLER = WM_APP + 1;
  WM_TRIGGER = WM_APP + 2;

type
  TState = (stIdle, stSimple, stSendMessageInHandler, stDestroyInHandler,
    stDestroyInHandler2, stReleaseInHandler, stRelease, stCount, stDestroy,
    stBlock, stRegisterInHandler, stUnregisterSelfInHandler,
    stUnRegisterOtherInHandler);
  THandler = (hdBefore1, hdBefore2, hdAfter1, hdAfter2);

  TJvWndProcHookTestCase1 = class(TTestCase)
  private
    FState: TState;
    FSubState: Integer;
    FCount: Integer;
  protected
    procedure EventHandler(Sender: TObject; const Event: TFormEvent);
  public
    procedure SetUp; override;
    procedure TearDown; override;

    function BeforeHandler1(var Msg: TMessage): Boolean;
    function AfterHandler1(var Msg: TMessage): Boolean;

    function BeforeHandler2(var Msg: TMessage): Boolean;
    function AfterHandler2(var Msg: TMessage): Boolean;

    procedure Wait(const MilliSecs: Integer);
    procedure InitForm;
    procedure PostTrigger(const AParam: Integer = 0);

    procedure CheckCount(const ACount: Integer; const Msg: string = '');
    procedure CheckFormExists;
    procedure CheckFormDoesNotExists;

    procedure RegisterHandler(const AHandler: THandler; const ExpectedResult: Boolean = True);
    procedure UnRegisterHandler(const AHandler: THandler; const ExpectedResult: Boolean = True);
  published
    procedure Test_Simple;
    procedure Test_Simple2;
    procedure Test_Destroy;
    procedure Test_Release;
    procedure Test_DestroyInHandler;
    procedure Test_DestroyInHandler2;
    procedure Test_SendMessageInHandler;
    procedure Test_ReleaseInHandler;
    procedure Test_Block;
    procedure Test_UnRegisterSelfInHandler;
    procedure Test_UnRegisterOtherInHandler;
    procedure Test_RegisterInHandler;
    procedure Test_UnRegister;
    procedure Test_RecreateWnd;
  end;

implementation

const
  CHandler: array[THandler] of string = ('BeforeHandler1', 'BeforeHandler2',
    'AfterHandler1', 'AfterHandler2');
  CBool: array[Boolean] of string = ('False', 'True');

  { TJvWndProcHookTestCase1 }

function TJvWndProcHookTestCase1.AfterHandler1(var Msg: TMessage): Boolean;
begin
  Result := False;
  case FState of
    stBlock:
      if Msg.Msg = WM_TRIGGER then
      begin
        Inc(FCount);
        if FSubState = 2 then
          Result := True;
      end;
    stCount, stDestroyInHandler, stReleaseInHandler:
      if Msg.Msg = WM_TRIGGER then
        Inc(FCount);
    stDestroy:
      if Msg.Msg = WM_DESTROY then
        Inc(FCount);
    stRelease:
      if Msg.Msg = CM_RELEASE then
        Inc(FCount);
    stRegisterInHandler:
      if Msg.Msg = WM_TRIGGER then
      begin
        Inc(FCount);
        RegisterHandler(hdAfter2);
      end;
    stSendMessageInHandler:
      case Msg.Msg of
        WM_TRIGGER, WM_SENDINHANDLER:
          Inc(FCount);
      end;
    stUnregisterSelfInHandler:
      if Msg.Msg = WM_TRIGGER then
      begin
        Inc(FCount);
        if Msg.WParam = 3 then
          UnRegisterHandler(hdAfter1);
      end;
    stUnRegisterOtherInHandler:
      if Msg.Msg = WM_TRIGGER then
      begin
        Inc(FCount);
        UnRegisterHandler(hdAfter2);
      end;
  else
    {}
  end;
end;

function TJvWndProcHookTestCase1.AfterHandler2(var Msg: TMessage): Boolean;
begin
  Result := False;
  case FState of
    stBlock:
      if Msg.Msg = WM_TRIGGER then
      begin
        Inc(FCount);
        if FSubState = 3 then
          Result := True;
      end;
    stCount, stRegisterInHandler:
      if Msg.Msg = WM_TRIGGER then
        Inc(FCount);
    stDestroy:
      if Msg.Msg = WM_DESTROY then
        Inc(FCount);
    stDestroyInHandler:
      if Msg.Msg = WM_TRIGGER then
        Inc(FCount);
    stRelease:
      if Msg.Msg = CM_RELEASE then
        Inc(FCount);
    stReleaseInHandler:
      if Msg.Msg = WM_TRIGGER then
        Inc(FCount);
    stSendMessageInHandler:
      case Msg.Msg of
        WM_TRIGGER, WM_SENDINHANDLER:
          Inc(FCount);
      end;
    stUnregisterSelfInHandler:
      if Msg.Msg = WM_TRIGGER then
      begin
        Inc(FCount);
        if Msg.WParam = 4 then
          UnRegisterHandler(hdAfter2);
      end;
    stUnRegisterOtherInHandler:
      if Msg.Msg = WM_TRIGGER then
        Inc(FCount);
  else
    {}
  end;
end;

function TJvWndProcHookTestCase1.BeforeHandler1(var Msg: TMessage): Boolean;
begin
  Result := False;
  case FState of
    stDestroy:
      if Msg.Msg = WM_DESTROY then
        Inc(FCount);
    stBlock:
      case Msg.Msg of
        WM_TRIGGER:
          begin
            Inc(FCount);
            if FSubState = 0 then
              Result := True;
          end;
        CM_RELEASE:
          Result := True;
      end;
    stCount:
      if Msg.Msg = WM_TRIGGER then
        Inc(FCount);
    stSimple:
      begin
        Check(Msg.Msg = WM_TEST, 'Not WM_TEST');
        Check(Msg.WParam = 0, 'Not 0');
        Check(Msg.LParam = 0, 'Not 0');
      end;
    stSendMessageInHandler:
      case Msg.Msg of
        WM_TRIGGER:
          begin
            Inc(FCount);
            SendMessage(TSimpleFrm.Instance.Handle, WM_SENDINHANDLER, 0, 0);
          end;
        WM_SENDINHANDLER:
          Inc(FCount);
      end;
    stDestroyInHandler:
      if Msg.Msg = WM_TRIGGER then
      begin
        Inc(FCount);
        CheckFormExists;
        TSimpleFrm.Final;
        CheckFormDoesNotExists;
      end;
    stDestroyInHandler2:
      if Msg.Msg = WM_TRIGGER then
      begin
        Inc(FCount);
        CheckFormExists;
        TSimpleFrm.Final;
        CheckFormDoesNotExists;
        Wait(1000);
        CheckFormDoesNotExists;
      end;
    stReleaseInHandler:
      if Msg.Msg = WM_TRIGGER then
      begin
        Inc(FCount);
        CheckFormExists;
        TSimpleFrm.Instance.Release;
      end;
    stRelease:
      if Msg.Msg = CM_RELEASE then
        Inc(FCount);
    stRegisterInHandler:
      if Msg.Msg = WM_TRIGGER then
      begin
        Inc(FCount);
        RegisterHandler(hdBefore2);
      end;
    stUnregisterSelfInHandler:
      if Msg.Msg = WM_TRIGGER then
      begin
        Inc(FCount);
        if Msg.WParam = 1 then
          UnRegisterHandler(hdBefore1);
      end;
    stUnRegisterOtherInHandler:
      if Msg.Msg = WM_TRIGGER then
      begin
        Inc(FCount);
        UnRegisterHandler(hdBefore2);
      end;
  else
    {}
  end;
end;

function TJvWndProcHookTestCase1.BeforeHandler2(
  var Msg: TMessage): Boolean;
begin
  Result := False;
  case FState of
    stBlock:
      if Msg.Msg = WM_TRIGGER then
      begin
        Inc(FCount);
        if FSubState = 1 then
          Result := True;
      end;
    stCount, stReleaseInHandler:
      if Msg.Msg = WM_TRIGGER then
        Inc(FCount);
    stDestroy:
      if Msg.Msg = WM_DESTROY then
        Inc(FCount);
    stRegisterInHandler:
      if Msg.Msg = WM_TRIGGER then
      begin
        Inc(FCount);
        RegisterHandler(hdAfter1);
      end;
    stRelease:
      if Msg.Msg = CM_RELEASE then
        Inc(FCount);
    stSendMessageInHandler:
      case Msg.Msg of
        WM_TRIGGER, WM_SENDINHANDLER:
          Inc(FCount);
      end;
    stUnregisterSelfInHandler:
      if Msg.Msg = WM_TRIGGER then
      begin
        Inc(FCount);
        if Msg.WParam = 2 then
          UnRegisterHandler(hdBefore2);
      end;
    stUnRegisterOtherInHandler:
      if Msg.Msg = WM_TRIGGER then
      begin
        Inc(FCount);
        UnRegisterHandler(hdAfter1);
      end;
  else
    {}
  end;
end;

procedure TJvWndProcHookTestCase1.CheckCount(const ACount: Integer;
  const Msg: string);
begin
  Check(FCount = ACount, Format('FCount=%d, expected=%d; %s',
    [FCount, ACount, Msg]));
end;

procedure TJvWndProcHookTestCase1.CheckFormDoesNotExists;
begin
  Check(not TSimpleFrm.InstanceAllocated, 'Form still exists');
end;

procedure TJvWndProcHookTestCase1.CheckFormExists;
begin
  Check(TSimpleFrm.InstanceAllocated, 'Form does not exist');
end;

procedure TJvWndProcHookTestCase1.EventHandler(Sender: TObject;
  const Event: TFormEvent);
begin
  case Event of
    feOnClose: ;
    feOnCreate: ;
    feOnDestroy:
      begin
        { Check if RegisterWndProcHook fails because now the form is destroying }
        RegisterHandler(hdBefore1, False);
        RegisterHandler(hdBefore2, False);
        RegisterHandler(hdAfter1, False);
        RegisterHandler(hdAfter2, False);
      end;
    feOnShow: ;
  end;
end;

procedure TJvWndProcHookTestCase1.InitForm;
begin
  TSimpleFrm.Execute;

  Wait(300);

  FState := stIdle;
  FCount := 0;
end;

procedure TJvWndProcHookTestCase1.PostTrigger(const AParam: Integer);
begin
  PostMessage(TSimpleFrm.Instance.Handle, WM_TRIGGER, AParam, 0);
end;

procedure TJvWndProcHookTestCase1.RegisterHandler(const AHandler: THandler;
  const ExpectedResult: Boolean);
var
  Result: Boolean;
begin
  case AHandler of
    hdBefore1:
      Result := RegisterWndProcHook(TSimpleFrm.Instance, BeforeHandler1, hoBeforeMsg);
    hdBefore2:
      Result := RegisterWndProcHook(TSimpleFrm.Instance, BeforeHandler2, hoBeforeMsg);
    hdAfter1:
      Result := RegisterWndProcHook(TSimpleFrm.Instance, AfterHandler1, hoAfterMsg);
    hdAfter2:
      Result := RegisterWndProcHook(TSimpleFrm.Instance, AfterHandler2, hoAfterMsg);
  else
    Result := False;
    Check(False, 'RegisterHandler');
  end;

  Check(Result = ExpectedResult, Format('RegisterWndProcHook %s returns %s',
    [CHandler[AHandler], CBool[Result]]));
end;

procedure TJvWndProcHookTestCase1.SetUp;
begin
  TSimpleFrm.RegisterClient(Self, nil, EventHandler);
end;

procedure TJvWndProcHookTestCase1.TearDown;
begin
  TSimpleFrm.UnRegisterClient(Self);
end;

procedure TJvWndProcHookTestCase1.Test_Block;
var
  I: Integer;
begin
  { Test if messages can be blocked for other hooks

    * We register 4 hooks on the window.
    * Substate 0: BeforeHandler1 blocks message (Count = 1)
    * Substate 1: BeforeHandler2 blocks message (Count = 2)
    etc.
  }

  InitForm;

  RegisterHandler(hdBefore1);
  RegisterHandler(hdBefore2);
  RegisterHandler(hdAfter1);
  RegisterHandler(hdAfter2);

  FState := stBlock;

  for I := 0 to 3 do
  begin
    FSubState := I;
    FCount := 0;
    PostTrigger;

    Wait(100);

    CheckCount(I + 1, Format('(Step %d)', [I]));
  end;

  CheckFormExists;

  TSimpleFrm.Instance.Release;

  Wait(500);

  CheckFormExists;

  UnRegisterHandler(hdBefore1);
  UnRegisterHandler(hdBefore2);
  UnRegisterHandler(hdAfter1);
  UnRegisterHandler(hdAfter2);

  Wait(500);

  TSimpleFrm.Final;
end;

procedure TJvWndProcHookTestCase1.Test_Destroy;
begin
  { Test if we can destroy a hooked control.

    * We register 4 hooks on the window.
    * Destroy the form.
    * All handlers should receive the WM_DESTROY (destruction of the Handle)
    * No errors should occur.
  }

  InitForm;

  RegisterHandler(hdBefore1);
  RegisterHandler(hdBefore2);
  RegisterHandler(hdAfter1);
  RegisterHandler(hdAfter2);

  FState := stDestroy;

  TSimpleFrm.Final;

  Wait(500);

  CheckCount(4);

  CheckFormDoesNotExists;
end;

procedure TJvWndProcHookTestCase1.Test_DestroyInHandler;
begin
  { Test if we can destroy a hooked control while we are handling a message
    in a hook on that control:

    * We register 4 hooks on the window.
    * Send the trigger.
    * BeforeHandler1 responds on the trigger by freeing the form.
    * Only BeforeHandler1 should receive the trigger, thus Count should be 1.
  }

  InitForm;

  RegisterHandler(hdBefore1);
  RegisterHandler(hdBefore2);
  RegisterHandler(hdAfter1);
  RegisterHandler(hdAfter2);

  FState := stDestroyInHandler;

  PostTrigger;

  Wait(500);

  CheckCount(1);

  CheckFormDoesNotExists;
end;

procedure TJvWndProcHookTestCase1.Test_DestroyInHandler2;
begin
  { Test if we can destroy a hooked control while we are handling a message
    in a hook on that control:

    * We register 4 hooks on the window.
    * Send the trigger.
    * BeforeHandler1 responds on the trigger by freeing the form.
    * Only BeforeHandler1 should receive the trigger, thus Count should be 1.

    -- This test currently fails --
  }

  InitForm;
  try
    RegisterHandler(hdBefore1);

    FState := stDestroyInHandler2;

    PostTrigger;

    Wait(500);

    CheckCount(1);

    CheckFormDoesNotExists;
  finally
    TSimpleFrm.Final;
  end;
end;

procedure TJvWndProcHookTestCase1.Test_RecreateWnd;
begin
  { Test if the hooks survives a recreation of the window }

  InitForm;

  RegisterHandler(hdBefore1);
  RegisterHandler(hdBefore2);
  RegisterHandler(hdAfter1);
  RegisterHandler(hdAfter2);

  FState := stCount;
  Wait(100);
  PostTrigger;
  Wait(100);
  CheckCount(4);

  FCount := 0;
  PostMessage(TSimpleFrm.Instance.Handle, CM_RECREATEWND, 0, 0);
  Wait(100);
  PostTrigger;
  Wait(100);
  CheckCount(4);

  UnRegisterHandler(hdBefore1);
  UnRegisterHandler(hdBefore2);
  UnRegisterHandler(hdAfter1);
  UnRegisterHandler(hdAfter2);

  TSimpleFrm.Final;
end;

procedure TJvWndProcHookTestCase1.Test_RegisterInHandler;
begin
  { Test if we can register another hook while we are handling a message
    in a hook:

    In the BeforeHandler1 handler we try to register BeforeHandler2, in the
    BeforeHandler2 handler we try to register AfterHandler1 etc.

    If everything works ok we should get a count of 4. (All handlers respond)
  }

  InitForm;
  try
    RegisterHandler(hdBefore1);

    FState := stRegisterInHandler;

    PostTrigger;

    Wait(200);

    { Returns 1 }
    CheckCount(4, '(1)');

    UnRegisterHandler(hdBefore1);
    UnRegisterHandler(hdBefore2);
    UnRegisterHandler(hdAfter1);
    UnRegisterHandler(hdAfter2);

  finally
    Wait(500);

    TSimpleFrm.Final;
  end;
end;

procedure TJvWndProcHookTestCase1.Test_Release;
begin
  { Test if we can release a hooked control.

    * We register 4 hooks on the window.
    * Release the form.
    * BeforeHandler1 and BeforeHandler2 should receive the CM_RELEASE,
      thus Count should be 2.
    * No errors should occur.
  }

  InitForm;

  RegisterHandler(hdBefore1);
  RegisterHandler(hdBefore2);
  RegisterHandler(hdAfter1);
  RegisterHandler(hdAfter2);

  FState := stRelease;

  TSimpleFrm.Instance.Release;

  Wait(200);

  CheckCount(2);

  Wait(200);

  CheckFormDoesNotExists;
end;

procedure TJvWndProcHookTestCase1.Test_ReleaseInHandler;
begin
  { Test if we can release a hooked control while we are handling a message
    in a hook on that control:

    * We register 4 hooks on the window.
    * Send the trigger.
    * BeforeHandler1 responds on the trigger by releasing the form.
    * All hooks should receive the trigger, thus Count should be 4.
  }

  InitForm;

  RegisterHandler(hdBefore1);
  RegisterHandler(hdBefore2);
  RegisterHandler(hdAfter1);
  RegisterHandler(hdAfter2);

  FState := stReleaseInHandler;

  PostTrigger;

  Wait(500);

  CheckCount(4);

  CheckFormDoesNotExists;
end;

procedure TJvWndProcHookTestCase1.Test_SendMessageInHandler;
begin
  { Test if we can send a message while we are handling a message
    in that hook:

    * We register 4 hooks on the window.
    * BeforeHandler1 sends a message to the form on receive of the trigger.
    * All hooks increase the count on receive off the trigger/message send on trigger
    * Count should thus be 8.
  }

  InitForm;

  RegisterHandler(hdBefore1);
  RegisterHandler(hdBefore2);
  RegisterHandler(hdAfter1);
  RegisterHandler(hdAfter2);

  FState := stSendMessageInHandler;

  PostTrigger;

  Wait(500);

  CheckCount(8);

  UnRegisterHandler(hdBefore1);
  UnRegisterHandler(hdBefore2);
  UnRegisterHandler(hdAfter1);
  UnRegisterHandler(hdAfter2);

  Wait(500);

  TSimpleFrm.Final;
end;

procedure TJvWndProcHookTestCase1.Test_Simple;
var
  Result: Boolean;
begin
  Result := RegisterWndProcHook(nil, BeforeHandler1, hoBeforeMsg);
  Check(not Result, 'Control = nil, RegisterWndProcHook returns True');

  InitForm;

  RegisterHandler(hdBefore1);

  Wait(500);

  FState := stSimple;
  SendMessage(TSimpleFrm.Instance.Handle, WM_TEST, 0, 0);
  FState := stIdle;

  Wait(500);

  UnRegisterHandler(hdBefore1);

  Wait(500);

  TSimpleFrm.Final;
end;

procedure TJvWndProcHookTestCase1.Test_Simple2;
begin
  InitForm;

  RegisterHandler(hdBefore1);
  RegisterHandler(hdBefore2);
  RegisterHandler(hdAfter1);
  RegisterHandler(hdAfter2);

  FState := stCount;

  PostTrigger;

  Wait(500);

  CheckCount(4);

  UnRegisterHandler(hdBefore1);
  UnRegisterHandler(hdBefore2);
  UnRegisterHandler(hdAfter1);
  UnRegisterHandler(hdAfter2);

  Wait(500);

  TSimpleFrm.Final;
end;

procedure TJvWndProcHookTestCase1.Test_UnRegister;
begin
  { Simple test that checks whether we can register/unregister hooks
    in the same destroy order as create order
  }

  InitForm;

  RegisterHandler(hdBefore1);
  RegisterHandler(hdBefore2);
  RegisterHandler(hdAfter1);
  RegisterHandler(hdAfter2);

  FState := stCount;

  FCount := 0;
  PostTrigger;
  Wait(100);

  CheckCount(4, '(Step 1)');

  UnRegisterHandler(hdBefore1);

  Wait(100);
  FCount := 0;
  PostTrigger;
  Wait(100);

  CheckCount(3, '(Step 2)');

  UnRegisterHandler(hdBefore2);

  Wait(100);
  FCount := 0;
  PostTrigger;
  Wait(100);

  CheckCount(2, '(Step 3)');

  UnRegisterHandler(hdAfter1);

  Wait(100);
  FCount := 0;
  PostTrigger;
  Wait(100);

  CheckCount(1, '(Step 4)');

  UnRegisterHandler(hdAfter2);

  Wait(100);
  FCount := 0;
  PostTrigger;
  Wait(100);

  CheckCount(0, '(Step 5)');

  Wait(500);

  TSimpleFrm.Final;
end;

procedure TJvWndProcHookTestCase1.Test_UnRegisterOtherInHandler;
begin
  { Test if we can unregister another hook while we are handling a message
    in a hook:

    In the BeforeHandler1 handler we try to unregister BeforeHandler2, in the
    BeforeHandler2 handler we try to unregister AfterHandler1 etc.

    If everything works ok we should get a count of 2. (BeforeHandler1 and
    AfterHandler1)
  }

  InitForm;
  try

    RegisterHandler(hdBefore1);
    RegisterHandler(hdBefore2);
    RegisterHandler(hdAfter1);
    RegisterHandler(hdAfter2);

    FState := stUnRegisterOtherInHandler;

    PostTrigger;

    Wait(300);
    CheckCount(2);

    UnRegisterHandler(hdBefore1);
    UnRegisterHandler(hdBefore2);
    UnRegisterHandler(hdAfter1);
    UnRegisterHandler(hdAfter2);

  finally
    TSimpleFrm.Final;
  end;
end;

procedure TJvWndProcHookTestCase1.Test_UnRegisterSelfInHandler;
var
  I: Integer;
begin
  { Test if we can unregister a hook while we are handling a message
    in that hook:

    * We register 4 hooks on the window.
    * Unregister BeforeHandler1 when it receives the trigger(1). (Count = 4)
    * Unregister BeforeHandler2 when it receives the trigger(2). (Count = 3)
    etc.
  }

  InitForm;

  RegisterHandler(hdBefore1);
  RegisterHandler(hdBefore2);
  RegisterHandler(hdAfter1);
  RegisterHandler(hdAfter2);

  FState := stUnregisterSelfInHandler;

  for I := 1 to 4 do
  begin
    FCount := 0;

    PostTrigger(I);
    Wait(200);
    CheckCount(5 - I, Format('(Step %d)', [I]));
    Wait(200);
  end;

  FCount := 0;
  PostTrigger;
  CheckCount(0, '(Step 5)');
  Wait(200);

  UnRegisterHandler(hdBefore1);
  UnRegisterHandler(hdBefore2);
  UnRegisterHandler(hdAfter1);
  UnRegisterHandler(hdAfter2);

  Wait(500);

  TSimpleFrm.Final;
end;

procedure TJvWndProcHookTestCase1.UnRegisterHandler(
  const AHandler: THandler; const ExpectedResult: Boolean);
var
  Result: Boolean;
begin
  Check(TSimpleFrm.InstanceAllocated, 'UnRegisterHandler: No instance');
  case AHandler of
    hdBefore1:
      Result := UnRegisterWndProcHook(TSimpleFrm.Instance, BeforeHandler1, hoBeforeMsg);
    hdBefore2:
      Result := UnRegisterWndProcHook(TSimpleFrm.Instance, BeforeHandler2, hoBeforeMsg);
    hdAfter1:
      Result := UnRegisterWndProcHook(TSimpleFrm.Instance, AfterHandler1, hoAfterMsg);
    hdAfter2:
      Result := UnRegisterWndProcHook(TSimpleFrm.Instance, AfterHandler2, hoAfterMsg);
  else
    Result := False;
    Check(False, 'UnRegisterHandler');
  end;

  Check(Result = ExpectedResult, Format('UnRegisterWndProcHook %s returns %s',
    [CHandler[AHandler], CBool[Result]]));
end;

procedure TJvWndProcHookTestCase1.Wait(const MilliSecs: Integer);
var
  EndTime: TDateTime;
begin
  EndTime := Now + MilliSecs / (24 * 60 * 60 * 1000);
  while Now < EndTime do
    Application.ProcessMessages;
end;

initialization
  TestFramework.RegisterTest(TJvWndProcHookTestCase1.Suite);
end.

