unit JvInterpreterSimpleExpressionFm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvExStdCtrls, JvEdit, JvValidateEdit, StdCtrls, JvComponentBase,
  JvInterpreter;

type
  TForm1 = class(TForm)
    JvInterpreterProgram1: TJvInterpreterProgram;
    Button1: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    EditA: TJvValidateEdit;
    EditB: TJvValidateEdit;
    Label3: TLabel;
    EditC: TJvValidateEdit;
    Label4: TLabel;
    Label5: TLabel;
    Edit2: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure JvInterpreterProgram1GetValue(Sender: TObject;
      Identifier: String; var Value: Variant; Args: TJvInterpreterArgs;
      var Done: Boolean);
    procedure JvInterpreterProgram1SetValue(Sender: TObject;
      Identifier: String; const Value: Variant; Args: TJvInterpreterArgs;
      var Done: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
uses Math;

procedure TForm1.Button1Click(Sender: TObject);
begin
  { BEGIN
      RESULT := <EXPRESSION>;
    END

    Note: Any time a variable or function name is invoked, the GetValue event is fired.... 
  }
  JvInterpreterProgram1.Source := 'begin'+Chr(13)+
                                     'result := '+ Edit1.Text+';'+Chr(13)+
                                  'end;';
  JvInterpreterProgram1.Run;

  Edit2.Text :=  VarToStr( JvInterpreterProgram1.VResult);
end;

procedure TForm1.JvInterpreterProgram1GetValue(Sender: TObject;
  Identifier: String; var Value: Variant; Args: TJvInterpreterArgs;
  var Done: Boolean);
begin
  Identifier := UpperCase(Identifier);
  { This event is fired by JvInterpreter not only to get variable names (A=5)
    but also to evaluate expressions like Foo(3), where Foo might be a function
    defined by you, with parameters.  Args contains those parameters. }

  { you would not typically do a brute-force block of if-else statements here in a
   real program,  but would do some kind of more elegant lookup in a table of your
   own named data objects that are used for your logic, to look up variables
   and function names. }
  if Identifier='A' then
  begin
      Value := EditA.Value;
      Done := true;  {VERY IMPORTANT!}
  end
  else
  if Identifier='B' then
  begin
      Value := EditB.Value;
      Done := true; {VERY IMPORTANT!}
  end
  else
  if Identifier='C' then
  begin
      Value := EditC.Value;
      Done := true; {VERY IMPORTANT!}
  end
  else { FUNCTION DEMO! }
  if (Identifier='MAX') then begin
      if (Args.Count=2) and
         VarIsNumeric(Args.Values[0]) and
          VarIsNumeric(Args.Values[1])
         then begin
           Value := Max(Args.Values[0],Args.Values[1]);
           Done := true; {VERY IMPORTANT!}
         end else begin
            { You can raise exceptions if invalid parameters are provided, or just let the default
             'not found' error get raised.}
            JvInterpreterError(ieIncompatibleTypes,0); // or  ieNotEnoughParams, or others. 
         end;
  end;

end;

procedure TForm1.JvInterpreterProgram1SetValue(Sender: TObject;
  Identifier: String; const Value: Variant; Args: TJvInterpreterArgs;
  var Done: Boolean);
begin
  if VarIsNumeric(Value) then begin
    if Identifier='A' then
    begin
        EditA.Value := Value;
        Done := true;
    end
    else
    if Identifier='B' then
    begin
        EditB.Value := Value;
        Done := true;
    end
    else if Identifier='C' then
    begin
        EditC.Value := Value;
        Done := true;
    end;
  end;
end;

end.
