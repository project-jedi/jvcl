unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvBehaviorLabel;

type
  TForm1 = class(TForm)
    lblCodeBreaker: TJvBehaviorLabel;
    Button1: TButton;
    lblAppearing: TJvBehaviorLabel;
    Button2: TButton;
    lblBlinking: TJvBehaviorLabel;
    Button3: TButton;
    lblBouncing: TJvBehaviorLabel;
    Button4: TButton;
    lblScrolling: TJvBehaviorLabel;
    Button5: TButton;
    lblSpecial: TJvBehaviorLabel;
    Button6: TButton;
    lblTyping: TJvBehaviorLabel;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoCodeBreakStart(Sender:TObject);
    procedure DoCodeBreakStop(Sender:TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  lblCodeBreaker.OnStart := nil;
  lblCodeBreaker.OnStop := nil;
  // this might trigger the OnStart/OnStop events, so set to nil
  lblCodeBreaker.BehaviorOptions.Active := not lblCodeBreaker.BehaviorOptions.Active;
  lblCodeBreaker.OnStart := DoCodeBreakStart;
  lblCodeBreaker.OnStop := DoCodeBreakStop;
end;

procedure TForm1.DoCodeBreakStart(Sender: TObject);
begin
  lblCodeBreaker.Caption := 'BREAK THE CODE';
end;

procedure TForm1.DoCodeBreakStop(Sender: TObject);
begin
  ShowMessage('Congratulations! You''ve hacked the system!');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  lblAppearing.BehaviorOptions.Active := not lblAppearing.BehaviorOptions.Active; 
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  lblBlinking.BehaviorOptions.Active := not lblBlinking.BehaviorOptions.Active;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  lblBouncing.BehaviorOptions.Active := not lblBouncing.BehaviorOptions.Active;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  lblScrolling.BehaviorOptions.Active := not lblScrolling.BehaviorOptions.Active;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  lblSpecial.BehaviorOptions.Active := not lblSpecial.BehaviorOptions.Active;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  lblTyping.BehaviorOptions.Active := not lblTyping.BehaviorOptions.Active;
end;

end.
