{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit ApptEditUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, JvTFManager, ExtCtrls;

type
  TApptEdit = class(TForm)
    Label1: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    StartDatePicker: TDateTimePicker;
    StartTimePicker: TDateTimePicker;
    EndDatePicker: TDateTimePicker;
    EndTimePicker: TDateTimePicker;
    AlarmEnabledCheck: TCheckBox;
    AlarmAdvanceEdit: TEdit;
    UpDown1: TUpDown;
    CancelButton: TButton;
    OKButton: TButton;
    Edit1: TEdit;
    Label6: TLabel;
    Bevel1: TBevel;
    Image1: TImage;
    Bevel2: TBevel;
    Image2: TImage;
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    // This form will use this var to indicate whether the appt is being
    // editing or whether is it being created.  (This is important in the
    // OnClose event.)
    AddingAppt : Boolean;
  public
    { Public declarations }
    // the main form will set this var to the selected appt if the user
    // wants to edit the appt OR the main form will set this var to nil
    // if the user wants to add a new appointment.
    Appt : TJvTFAppt;
  end;

var
  ApptEdit: TApptEdit;

implementation

Uses PhotoOpUnit, JvTFDays;

{$R *.DFM}

procedure TApptEdit.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  // Handles the Alarm Advance "spin"
  AlarmAdvanceEdit.Text := IntToStr(UpDown1.Position);
end;

procedure TApptEdit.FormShow(Sender: TObject);
var
  ApptStartDate,
  ApptEndDate : TDate;
  ApptStartTime,
  ApptEndTime : TTime;
  DaysGrid : TJvTFDays;
begin
  // Just a short cut to save typing :)
  DaysGrid := PhotoOpMain.JvTFDays1;

  If Assigned(Appt) Then
    Begin
      // Assume we want to edit the selected appt
      // Populate the window fields from the selected appt
      StartDatePicker.Date := Appt.StartDate;
      StartTimePicker.Time := Appt.StartTime;
      EndDatePicker.Date := Appt.EndDate;
      EndTimePicker.Time := Appt.EndTime;
      AlarmEnabledCheck.Checked := Appt.AlarmEnabled;
      AlarmAdvanceEdit.Text := IntToStr(Appt.AlarmAdvance);
      UpDown1.Position := Appt.AlarmAdvance;
      Edit1.Text := Appt.Description;
      // Change the caption of the form to indicate that the appointment
      // is being edited.
      Caption := 'Edit Appointment';
      // Set the AddingAppt var to false so that the form will recognize
      // that the appt is being edited and not created.
      AddingAppt := False;
    End
  Else
    Begin
      // Assume we are adding a new appt
      // Request an appt from the server
      Appt := PhotoOpMain.JvTFDays1.ScheduleManager.dbNewAppt('');
      // Right now this appt object is in a state of flux.  It is not
      // assigned to any schedules and shouldn't be because we're unsure
      // of its data.  The caching system is programmed to automatically
      // destroy any appt objects that are not assigned to any schedules.
      // In this particular situation, this is bad.
      // So...
      // Make the Appt object persistent so the cache does not attempt to
      // flush it while this window is open.
      Appt.Persistent := True;

      // Now populate the appt object with some default data which is
      // retrieved from the JvTFDays grid.
      With Appt do
        Begin
          If DaysGrid.ValidSelection Then
            Begin
              // Set the start/end dates according to the selection
              ApptStartDate := DaysGrid.Cols[DaysGrid.SelStart.X].SchedDate;
              ApptEndDate := DaysGrid.Cols[DaysGrid.SelEnd.X].SchedDate;
              // Set the start/end times according to the selection
              ApptStartTime := DaysGrid.RowToTime(DaysGrid.SelStart.Y);
              ApptEndTime := DaysGrid.RowEndTime(DaysGrid.SelEnd.Y);
            End
          Else
            Begin
              // Set the start/end dates to today
              ApptStartDate := Date;
              ApptEndDate := Date;
              // Set the start/end times to now and now + granularity
              ApptStartTime := Time;
                // Subtract one min from granularity, then add the minute back in.
                //  (Avoids minute overflow when granularity is 60)
              ApptEndTime := ApptStartTime +
                             EncodeTime(0, DaysGrid.Granularity - 1, 0, 0) +
                             EncodeTime(0, 1, 0, 0);
            End;

          Appt.BeginUpdate;
          // Call BeginUpdate so that the appt will NOT be posted when we
          // set the following properties.  Don't worry about calling
          // EndUpdate here.  It will be called if the user chooses 'OK' to
          // save the new appt.  See FormClose below.

          SetStartEnd(ApptStartDate, ApptStartTime, ApptEndDate, ApptEndTime);
          AlarmEnabled := True;
          AlarmAdvance := 15;
        End;

      // Now call this proc again to populate the window fields
      FormShow(nil);  // Appt won't be nil now so this call will fill the
                      // window fields
      // Set the caption of the form to indicate that the appt is being added
      Caption := 'Add Appointment';
      // Set the AddingAppt var to true so that the form will recognize that
      // the appt is being added instead of edited.
      AddingAppt := True;
    End;
end;

procedure TApptEdit.FormClose(Sender: TObject; var Action: TCloseAction);
var
  DaysGrid : TJvTFDays;
  I : Integer;
begin
  If (ModalResult = mrOK) and Assigned(Appt) Then
    With Appt do
      Begin
        Appt.BeginUpdate;
        // Call BeginUpdate so that the appt is not posted while we are
        // setting its properties.

        Try
          // Copy the data from the window fields to the appt object
          SetStartEnd(StartDatePicker.Date, StartTimePicker.Time,
                      EndDatePicker.Date, EndTimePicker.Time);
          AlarmEnabled := AlarmEnabledCheck.Checked;
          AlarmAdvance := StrToInt(AlarmAdvanceEdit.Text);
          Description := Edit1.Text;

          If AddingAppt Then
            Begin
              // Just a shortcut to save some typing :-)
              DaysGrid := PhotoOpMain.JvTFDays1;

              // Add the appt to selected schedule(s)
              If DaysGrid.ValidSelection Then
                For I := DaysGrid.SelStart.X to DaysGrid.SelEnd.X do
                  Appt.AddSchedule(DaysGrid.Cols[I].SchedName);

              // Now that we're done working with the appointment and it is
              // actually assigned to a schedule, we should set the Persistent
              // property to false.  This will ensure the the caching system
              // will properly dispose of the appointment object when it is
              // no longer needed.
              Appt.Persistent := False;
            End;
        Finally
          Appt.EndUpdate;  // this causes the appt to be posted
        End;
      End
  Else If AddingAppt Then
    // The user canceled the window, but we have already created the appt
    // object.  We need to clean up the appt object we requested from the
    // server.  This can be done either by manually destroying the appt
    // object (Appt.Free), or by setting Appt.Persistent to false.
    Appt.Free;

  // Set Appt to nil to prepare window for next opening
  Appt := nil;
end;

end.
