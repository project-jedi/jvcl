unit FrHint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FrTypes, FrUtils, FrCommCl, StdCtrls, ExtCtrls;

type
  TFrCaption = class(TJvComponent)
  private
    FPrevWndProc: Pointer;
    FNewWndProc: Pointer;

    //    procedure SetFont( Value: TFont );
    //    procedure Repaint;
    procedure ParentWindowHookProc(var Msg_: TMessage);
    procedure SetParentWindowHook;
    procedure FreeParentWindowHook;
    //    procedure SmthChanged(Sender: TObject);
  protected
    //    procedure WndProc(var Message: TMessage);override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    //    property Parent: TForm read Fparent write SetParent;
  end;

procedure Register;

implementation

procedure Register;
begin

end;
//==============================================================

constructor TFrCaption.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetParentWindowHook;
  Repaint;
end;
//-----

destructor TFrCaption.Destroy;
begin
  FreeParentWindowHook;
  inherited Destroy;
end;
//=========================================================.special procs.

procedure TFrCaption.SetParentWindowHook;
var
  P: Pointer;
begin
  P := Pointer(GetWindowLong(TForm(Owner).Handle, GWL_WNDPROC));
  if (P <> FNewWndProc) then
  begin
    FPrevWndProc := P;
    FNewWndProc := MakeObjectInstance(ParentWindowHookProc);
    SetWindowLong(TForm(Owner).Handle, GWL_WNDPROC, LongInt(FNewWndProc));
  end;
end;
//==============================================================

procedure TFrCaption.FreeParentWindowHook;
begin
  if (FNewWndProc <> nil) and (FPrevWndProc <> nil)
    and (Pointer(GetWindowLong(TForm(Owner).Handle, GWL_WNDPROC)) = FNewWndProc) then
    SetWindowLong(TForm(Owner).Handle, GWL_WNDPROC, LongInt(FPrevWndProc));
  FNewWndProc := nil;
end;
//==============================================================

procedure TFrCaption.ParentWindowHookProc(var Msg_: TMessage);

  procedure DefaultProc; //___________________________________
  begin
    with Msg_ do
      Result := CallWindowProc(FPrevWndProc, TForm(Owner).Handle, Msg, WParam, LParam);
  end;

begin //_______________________________________________________
  with Msg_ do
    case Msg of
      WM_MOUSEMOVE:
        //      WM_MOUSEACTIVATE:
        begin
          DefaultProc;
        end;
      WM_DESTROY:
        begin
          FreeParentWindowHook;
          DefaultProc;
        end;
    else
      DefaultProc;
    end;
end;
//==============================================================

end.
