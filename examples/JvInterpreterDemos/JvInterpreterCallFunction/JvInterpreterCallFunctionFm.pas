unit JvInterpreterCallFunctionFm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponentBase, JvInterpreter, StdCtrls, JvExControls,
  JvEditorCommon, JvEditor, JvHLEditor;

type
  TForm1 = class(TForm)
    JvHLEditor1: TJvHLEditor;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    JvInterpreterProgram1: TJvInterpreterProgram;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure JvInterpreterProgram1GetValue(Sender: TObject;
      Identifier: String; var Value: Variant; Args: TJvInterpreterArgs;
      var Done: Boolean);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
 Args:TJvInterpreterArgs;
begin
  { before we get here, JvInterpreterProgram1.Pas is already set to show the text in the HLEditor. }
  Assert(JvInterpreterProgram1.Pas.Count>0);


  { THIS IS ONLY ONE POSSIBLE WAY TO CALL CallFunction! Look at both ways please. }

  {Args is a temporary argument data holder object}
  Args := TJvInterpreterArgs.Create;
  try
    Args.Count := 1;
    Args.Values[0] := 'SomeText';
    JvInterpreterProgram1.CallFunction( 'MyFunction', Args, []);
    { show result to user:}
    Edit1.Text := VarToStr( JvInterpreterProgram1.VResult );
  finally
    Args.Free;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  { move program text over to interpreter! }
 JvInterpreterProgram1.Pas.Assign( JvHLEditor1.Lines );
end;

procedure TForm1.JvInterpreterProgram1GetValue(Sender: TObject;
  Identifier: String; var Value: Variant; Args: TJvInterpreterArgs;
  var Done: Boolean);
begin
   Identifier := UpperCase(Identifier);

   if (Identifier='LENGTH') and (ARgs.Count=1) and (VarIsStr(Args.Values[0])) then
   begin
      Value := Length(ARgs.Values[0]);
      Done := true;
   end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
     Param1,Param2,Param3:Variant;
begin
  { before we get here, JvInterpreterProgram1.Pas is already set to show the text in the HLEditor. }
  Assert(JvInterpreterProgram1.Pas.Count>0);

     { Alternative method without creating/freeing JvInterpreter args is to use Params instead, but not Args:}

   Param1 :=  10;
   Param2 := 20;
   JvInterpreterProgram1.CallFunction( 'MyFunction2', nil, [Param1,Param2]  );


      { show result to user:}
  Edit1.Text := VarToStr( JvInterpreterProgram1.VResult );
end;


end.
