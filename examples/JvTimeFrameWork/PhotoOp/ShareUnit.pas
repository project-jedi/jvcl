unit ShareUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, checklst;

type
  TShare = class(TForm)
    ResourcesCheckList: TCheckListBox;
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Share: TShare;

implementation

Uses PhotoOpUnit, JvTFManager;

{$R *.DFM}

procedure TShare.FormShow(Sender: TObject);
var
  Appt : TJvTFAppt;
  I : Integer;
begin
  // First, get the selected appointment
  Appt := PhotoOpMain.JvTFDays1.SelAppt;

  // now roll through the resource list and check all resources
  // that are found in the appointment's list of schedules.
  With ResourcesCheckList do
    For I := 0 to Items.Count - 1 do
      Checked[I] := Appt.IndexOfSchedule(Items[I]) > -1;
end;

procedure TShare.FormClose(Sender: TObject; var Action: TCloseAction);
var
  TempList : TStringList;
  I : Integer;
begin
  If ModalResult = mrOK Then
    Begin
      // create and populate a temporary list of the selected resources
      // from the checklistbox.
      TempList := TStringList.Create;
      Try
        With ResourcesCheckList do
          For I := 0 to Items.Count - 1 do
            If Checked[I] Then
              TempList.Add(Items[I]);

        // Enforce a business rule where removing all resources from an
        // appointment causes the appointment to be deleted.
        // NOTE: This is NOT a requirement.  Feel free to implement any
        // business rules as you see fit.
        If TempList.Count > 0 Then
          // If at least one resource then change the appointment's
          // schedule list to match the temp list.
          PhotoOpMain.JvTFDays1.SelAppt.AssignSchedules(TempList)
        Else
          If MessageDlg('You have removed this appointment from all schedules.' +
                        '  This will cause the appointment to be deleted.' + #13#10 +
                        'Are you sure this is what you want to do?',
                        mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
            With PhotoOpMain.JvTFDays1 do
              // Delete the appointment if that is what the user wants to do.
              ScheduleManager.dbDeleteAppt(SelAppt)
          Else
            Action := caNone;

      Finally
        TempList.Free;
      End;
    End;
end;

end.
