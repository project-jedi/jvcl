unit MyLabel;

interface

uses Classes, StdCtrls;

type

   TMyLabel = class(TLabel)
   private
     function GetSomeProperty: String;
     procedure SetSomeProperty(Value: String);
   public
     procedure DoSomething;
     property SomeProperty: String read GetSomeProperty write SetSomeProperty;
   end;

  procedure Register;

implementation

{ TMyLabel }

procedure TMyLabel.DoSomething;
begin
  Caption := 'DoSomething';
end;

procedure Register;
begin
  RegisterComponents('JVCL', [TMyLabel]);
end;

function TMyLabel.GetSomeProperty: String;
begin
  Result := Caption;
end;

procedure TMyLabel.SetSomeProperty(Value: String);
begin
  Caption := Value;
end;

end.
