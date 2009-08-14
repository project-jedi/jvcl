{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

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

unit PhotoOpUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, ComCtrls, StdCtrls, Buttons, ExtCtrls,
  ImgList, JvTFManager, JvTFDays, JvTFGlance, JvTFGlanceTextViewer, JvTFMonths, JvTFWeeks,
  JvComponent, JvExControls;

type
  TPhotoOpMain = class(TForm)
    utfScheduleManager1: TJvTFScheduleManager;
    StateImageList: TImageList;
    NeedApptsQuery: TQuery;
    ApptSchedulesQuery: TQuery;
    GetApptQuery: TQuery;
    DeleteApptLinkQuery: TQuery;
    DeleteApptQuery: TQuery;
    SchedulesQuery: TQuery;
    JvTFDaysPrinter1: TJvTFDaysPrinter;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    JvTFDays1: TJvTFDays;
    JvTFWeeks1: TJvTFWeeks;
    JvTFMonths1: TJvTFMonths;
    GlanceTextViewer1: TJvTFGlanceTextViewer;
    GlanceTextViewer2: TJvTFGlanceTextViewer;
    Panel1: TPanel;
    ResourceCombo: TComboBox;
    PrevDateButton: TBitBtn;
    NextDateButton: TBitBtn;
    NewApptButton: TBitBtn;
    EditApptButton: TBitBtn;
    DeleteApptButton: TBitBtn;
    ViewSchedsButton: TBitBtn;
    HideSchedButton: TBitBtn;
    ShareButton: TBitBtn;
    TimeIncCombo: TComboBox;
    GotoDatePicker: TDateTimePicker;
    ModeCombo: TComboBox;
    DaysCombo: TComboBox;
    PrintButton: TBitBtn;
    dbUTF: TDatabase;
    procedure utfScheduleManager1PostAppt(Sender: TObject; Appt: TJvTFAppt);
    procedure utfScheduleManager1DeleteAppt(Sender: TObject; Appt: TJvTFAppt);
    procedure utfScheduleManager1RefreshAppt(Sender: TObject; Appt: TJvTFAppt);
    procedure ModeComboChange(Sender: TObject);
    procedure ViewSchedsButtonClick(Sender: TObject);
    procedure HideSchedButtonClick(Sender: TObject);
    procedure ResourceComboChange(Sender: TObject);
    procedure DaysComboChange(Sender: TObject);
    procedure ShareButtonClick(Sender: TObject);
    procedure PrevDateButtonClick(Sender: TObject);
    procedure NextDateButtonClick(Sender: TObject);
    procedure GotoDatePickerChange(Sender: TObject);
    procedure GotoDatePickerUserInput(Sender: TObject;
      const UserString: String; var DateAndTime: TDateTime;
      var AllowChange: Boolean);
    procedure TimeIncComboChange(Sender: TObject);
    procedure NewApptButtonClick(Sender: TObject);
    procedure EditApptButtonClick(Sender: TObject);
    procedure DeleteApptButtonClick(Sender: TObject);
    procedure JvTFDays1DateChanging(Sender: TObject; var NewDate: TDate);
    procedure JvTFDays1DateChanged(Sender: TObject);
    procedure JvTFDays1GranularityChanged(Sender: TObject);
    procedure JvTFDays1DblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PrintButtonClick(Sender: TObject);
    procedure JvTFDaysPrinter1ApptProgress(Sender: TObject; Current,
      Total: Integer);
    procedure JvTFDaysPrinter1AssembleProgress(Sender: TObject; Current,
      Total: Integer);
    procedure JvTFDaysPrinter1PrintProgress(Sender: TObject; Current,
      Total: Integer);
    procedure utfScheduleManager1LoadBatch(Sender: TObject; BatchName: String;
      BatchStartDate, BatchEndDate: TDate);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PhotoOpMain: TPhotoOpMain;

implementation

Uses VisibleResourcesUnit, ShareUnit, ApptEditUnit, PrintProgressUnit;

{$R *.DFM}

procedure TPhotoOpMain.utfScheduleManager1PostAppt(Sender: TObject;
  Appt: TJvTFAppt);
var
  I : Integer;
begin
  With GetApptQuery do
    Begin
      ParamByName('ApptID').AsString := Appt.ID;
      Open;

      If RecordCount > 0 Then // SQL RecordCount not reliable except on local tables
        Edit
      Else
        Begin
          Insert;
          FieldByName('ApptID').AsString := Appt.ID;
        End;

      FieldByName('StartDate').AsDateTime := Appt.StartDate;
      FieldByName('StartTime').AsDateTime := Appt.StartTime;
      FieldByName('EndDate').AsDateTime := Appt.EndDate;
      FieldByName('EndTime').AsDateTime := Appt.EndTime;
      FieldByName('Description').AsString := Appt.Description;
      FieldByName('AlarmEnabled').AsBoolean := Appt.AlarmEnabled;
      FieldByName('AlarmAdvance').AsInteger := Appt.AlarmAdvance;
      Post;
      Close;
    End;

  // Now update the Appt --> Schedule relationship
  // First delete all entries in the Link table
  With DeleteApptLinkQuery do
    Begin
      ParamByName('ApptID').AsString := Appt.ID;
      ExecSQL;
    End;
  // Now "refresh" the link table by adding a record for each of the names
  // in Appt.Schedules.  We will use the ApptSchedulesQuery to update the table.
  With ApptSchedulesQuery do
    Begin
      ParamByName('ApptID').AsString := Appt.ID;
      Open;
      For I := 0 to Appt.ScheduleCount - 1 do
        Begin
          Insert;
          FieldByName('ApptID').AsString := Appt.ID;
          FieldByName('SchedName').AsString := Appt.Schedules[I];
          Post;
        End;
      Close;
    End;
end;

procedure TPhotoOpMain.utfScheduleManager1DeleteAppt(Sender: TObject;
  Appt: TJvTFAppt);
begin
  // First delete the appointment from the appointment table
  With DeleteApptQuery do
    Begin
      ParamByName('ApptID').AsString := Appt.ID;
      ExecSQL;
    End;

  // Next, delete the related records from the link table
  With DeleteApptLinkQuery do
    Begin
      ParamByName('ApptID').AsString := Appt.ID;
      ExecSQL;
    End;
end;

procedure TPhotoOpMain.utfScheduleManager1RefreshAppt(Sender: TObject;
  Appt: TJvTFAppt);
begin
  With GetApptQuery do
    Begin
      ParamByName('ApptID').AsString := Appt.ID;
      Open;
      If RecordCount = 1 Then
        Begin
          Appt.SetStartEnd(FieldByName('StartDate').AsDateTime,
                           FieldByName('StartTime').AsDateTime,
                           FieldByName('EndDate').AsDateTime,
                           FieldByName('EndTime').AsDateTime);
          Appt.Description := FieldByName('Description').AsString;
          Appt.AlarmEnabled := FieldByName('AlarmEnabled').AsBoolean;
          Appt.AlarmAdvance := FieldByName('AlarmAdvance').AsInteger;
        End;
      Close;
    End;

  // Now update the Appt --> Schedule(s) relationship
  Appt.ClearSchedules;
  With ApptSchedulesQuery do
    Begin
      ParamByName('ApptID').AsString := Appt.ID;
      Open;
      First;
      While not EOF do
        Begin
          Appt.AddSchedule(FieldByName('SchedName').AsString);
          Next;
        End;
      Close; // ApptSchedulesQuery
    End;
end;

procedure TPhotoOpMain.ModeComboChange(Sender: TObject);
begin
  If ModeCombo.ItemIndex = 0 Then
    // Single mode
    Begin
      // display the appropriate tool bar controls
      ViewSchedsButton.Visible := False;
      HideSchedButton.Visible := False;
      ShareButton.Visible := False;
      ResourceCombo.Visible := True;
      DaysCombo.Visible := True;
      // synchronize the date
      JvTFDays1.Template.LinearStartDate := GotoDatePicker.Date;
      // "activate" the Linear template
      JvTFDays1.Template.ActiveTemplate := agtLinear;
      // set the column grouping
      JvTFDays1.Grouping := grResource;
    End
  Else
    // Group mode
    Begin
      // display the appropriate tool bar controls
      ViewSchedsButton.Visible := True;
      HideSchedButton.Visible := True;
      ShareButton.Visible := True;
      ResourceCombo.Visible := False;
      DaysCombo.Visible := False;
      // synchronize the date
      JvTFDays1.Template.CompDate := GotoDatePicker.Date;
      // "activate" the Comparative template
      JvTFDays1.Template.ActiveTemplate := agtComparative;
      // set the column grouping
      JvTFDays1.Grouping := grDate;
    End;
end;

procedure TPhotoOpMain.ViewSchedsButtonClick(Sender: TObject);
begin
  VisibleResources.ShowModal;
end;

procedure TPhotoOpMain.HideSchedButtonClick(Sender: TObject);
var
  I,
  NameIndex : Integer;
  NameList : TStringList;
begin
  NameList := TStringList.Create;

  Try
    With JvTFDays1 do
      Begin
        If ValidSelection Then
          Begin
            For I := SelStart.X to SelEnd.X do
              NameList.Add(Cols[I].SchedName);

            For I := 0 to NameList.Count - 1 do
              Begin
                NameIndex := Template.CompNames.IndexOf(NameList[I]);
                If NameIndex > -1 Then
                  Template.CompNames.Delete(NameIndex);
              End;
          End
        Else
          MessageDlg('Please select a schedule to hide.', mtInformation, [mbOK], 0);
      End;
  Finally
    NameList.Free;
  End;
end;

procedure TPhotoOpMain.ResourceComboChange(Sender: TObject);
begin
  JvTFDays1.Template.LinearName := ResourceCombo.Text;
  JvTFWeeks1.SchedNames.Clear;
  JvTFWeeks1.SchedNames.Add(ResourceCombo.Text);
  JvTFWeeks1.Refresh;
  JvTFMonths1.SchedNames.Clear;
  JvTFMonths1.SchedNames.Add(ResourceCombo.Text);
  JvTFMonths1.Refresh;
end;

procedure TPhotoOpMain.DaysComboChange(Sender: TObject);
begin
  Case DaysCombo.ItemIndex of
    0 : JvTFDays1.Template.LinearDayCount := 31;
    1 : JvTFDays1.Template.LinearDayCount := 14;
    2 : JvTFDays1.Template.LinearDayCount := 7;
    3 : JvTFDays1.Template.LinearDayCount := 5;
    4 : JvTFDays1.Template.LinearDayCount := 3;
    5 : JvTFDays1.Template.LinearDayCount := 2;
    6 : JvTFDays1.Template.LinearDayCount := 1;
  End;
end;

procedure TPhotoOpMain.ShareButtonClick(Sender: TObject);
begin
  If JvTFDays1.SelAppt <> nil Then
    Share.ShowModal
  Else
    MessageDlg('Please select an appointment.', mtInformation, [mbOK], 0);
end;

procedure TPhotoOpMain.PrevDateButtonClick(Sender: TObject);
begin
  JvTFDays1.PrevDate;
end;

procedure TPhotoOpMain.NextDateButtonClick(Sender: TObject);
begin
  JvTFDays1.NextDate;
end;

procedure TPhotoOpMain.GotoDatePickerChange(Sender: TObject);
begin
  // GotoDatePicker.OnCloseUp should also point to this handler
  JvTFDays1.GotoDate(GotoDatePicker.Date);
  JvTFWeeks1.DisplayDate := GotoDatePicker.Date;
  JvTFWeeks1.DisplayDate := GotoDatePicker.Date;
end;

procedure TPhotoOpMain.GotoDatePickerUserInput(Sender: TObject;
  const UserString: String; var DateAndTime: TDateTime;
  var AllowChange: Boolean);
begin
  AllowChange := True;
  GotoDatePicker.OnChange(nil);
end;

procedure TPhotoOpMain.TimeIncComboChange(Sender: TObject);
begin
  Case TimeIncCombo.ItemIndex of
     0 : JvTFDays1.Granularity := 60;
     1 : JvTFDays1.Granularity := 30;
     2 : JvTFDays1.Granularity := 20;
     3 : JvTFDays1.Granularity := 15;
     4 : JvTFDays1.Granularity := 12;
     5 : JvTFDays1.Granularity := 10;
     6 : JvTFDays1.Granularity := 6;
     7 : JvTFDays1.Granularity := 5;
     8 : JvTFDays1.Granularity := 4;
     9 : JvTFDays1.Granularity := 3;
    10 : JvTFDays1.Granularity := 2;
    11 : JvTFDays1.Granularity := 1;
  End;
end;

procedure TPhotoOpMain.NewApptButtonClick(Sender: TObject);
begin
  // Simply open the EditAppt window.  The Appt var of the
  // EditAppt form will already be nil (which indicates
  // that the appoinment is being created).
  ApptEdit.ShowModal;
end;

procedure TPhotoOpMain.EditApptButtonClick(Sender: TObject);
begin
  If Assigned(JvTFDays1.SelAppt) Then
    Begin
      // Set EditAppt's Appt var to the selected appointment to
      // indicate that the appointment should be edited.
      ApptEdit.Appt := JvTFDays1.SelAppt;
      ApptEdit.ShowModal;
    End
  Else
    MessageDlg('Please select an appointment to edit.', mtInformation,
               [mbOK], 0);
end;

procedure TPhotoOpMain.DeleteApptButtonClick(Sender: TObject);
var
  Appt : TJvTFAppt;
  dbDel : Boolean;
  SelSchedName : String;
begin
  // This routine employs a simple business that asks the user what to
  // do in the case where the user is attempting to delete a shared appt.
  // NOTE:  This is NOT required.  You are completely free to implement
  // any business rules you see fit.

  // Another shortcut to save typing
  Appt := JvTFDays1.SelAppt;

  If Assigned(Appt) Then
    Begin
      dbDel := True;
      If Appt.Shared Then
        If MessageDlg('This appointment is shared with other schedules.' + #13#10 +
                      'Do you want to delete the appointment from ' +
                      'all schedules?' + #13#10#13#10 +
                      'Choose ''No'' to delete the appointment from the ' +
                      'selected schedule only.' + #13#10 +
                      'Choose ''All'' to delete the appointment from all schedules.',
                      mtConfirmation, [mbNo, mbAll], 0) = mrNo Then
          Begin
            // Don't delete the appointment, but remove it from the schedule
            // of the selected resource.
            dbDel := False;

            With JvTFDays1 do
              Begin
                SelSchedName := '';
                If ValidSelection and Cols[SelStart.X].Connected Then
                  SelSchedName := Cols[SelStart.X].Schedule.SchedName;
              End;

            If SelSchedName <> '' Then
              Appt.RemoveSchedule(SelSchedName)
            Else
              MessageDlg('No schedule is selected.' + #13#10 +
                         'Could not remove appointment from schedule.',
                         mtInformation, [mbOK], 0);
          End;

      If dbDel Then
        If MessageDlg('Are you sure you want to delete the selected appointment?',
                      mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
          // Delete the appointment (removes it from the db)
          // Note: Could substitute Appt.Delete; for the line below
          JvTFDays1.ScheduleManager.dbDeleteAppt(JvTFDays1.SelAppt);
    End
  Else
    MessageDlg('Please select an appointment to delete.',
               mtInformation, [mbOK], 0);
end;

procedure TPhotoOpMain.JvTFDays1DateChanging(Sender: TObject;
  var NewDate: TDate);
begin
  // Make sure all appts are posted before moving on.
  JvTFDays1.ScheduleManager.PostAppts;
end;

procedure TPhotoOpMain.JvTFDays1DateChanged(Sender: TObject);
begin
  // Synchronize the tool bar
  With JvTFDays1.Template do
    If ActiveTemplate = agtLinear Then
      GotoDatePicker.Date := LinearStartDate
    Else
      GotoDatePicker.Date := CompDate;
end;

procedure TPhotoOpMain.JvTFDays1GranularityChanged(Sender: TObject);
begin
  // Update the TimeIncCombo when the granularity is changed.
  //  (This can be done by <Ctrl> + <Insert> and <Ctrl> + <Delete>)
  Case JvTFDays1.Granularity of
    60 : TimeIncCombo.ItemIndex := 0;
    30 : TimeIncCombo.ItemIndex := 1;
    20 : TimeIncCombo.ItemIndex := 2;
    15 : TimeIncCombo.ItemIndex := 3;
    12 : TimeIncCombo.ItemIndex := 4;
    10 : TimeIncCombo.ItemIndex := 5;
     6 : TimeIncCombo.ItemIndex := 6;
     5 : TimeIncCombo.ItemIndex := 7;
     4 : TimeIncCombo.ItemIndex := 8;
     3 : TimeIncCombo.ItemIndex := 9;
     2 : TimeIncCombo.ItemIndex := 10;
  Else
    TimeIncCombo.ItemIndex := 11;
  End;
end;

procedure TPhotoOpMain.JvTFDays1DblClick(Sender: TObject);
begin
  With JvTFDays1 do
    If ValidSelection Then
      If Assigned(SelAppt) Then
        EditApptButtonClick(nil)
      Else
        NewApptButtonClick(nil);
end;

procedure TPhotoOpMain.FormShow(Sender: TObject);
var
  ResName : String;
begin
  // Initialize the date
  //GotoDatePicker.Date := Date;
  GotoDatePicker.Date := EncodeDate(2002, 1, 1);

  // Initialize the granularity
  TimeIncCombo.ItemIndex := 1; // 30 mins

  // Initialize the mode
  ModeCombo.ItemIndex := 0; // Single mode
  DaysCombo.ItemIndex := 6; // One day

  // Populate the resource related controls
  With SchedulesQuery do
    try
      Open;
      First;
      While not EOF do
        Begin
          ResName := SchedulesQuery.FieldByName('SchedName').AsString;
          ResourceCombo.Items.Add(ResName);
          VisibleResources.ResourcesCheckList.Items.Add(ResName);
          Share.ResourcesCheckList.Items.Add(ResName);
          Next;
        End;
      Close;
    except
      on E:EDBEngineError do
      begin
        ShowMessageFmt('%s:'#13#10'Try moving the database to a shorter path.',[E.Message]);
        Application.Terminate;
        Exit;
      end;
    end;

  // Initialize the resource related controls
  ResourceCombo.ItemIndex := 0;
  VisibleResources.ResourcesCheckList.Checked[0] := True;

  // Initialize the comparative template
  JvTFDays1.Template.CompNames.Add(VisibleResources.ResourcesCheckList.Items[0]);

  // Now run the events to synchronize JvTFDays, etc.
  ResourceComboChange(nil);
  DaysComboChange(nil);
  ModeComboChange(nil);
  GotoDatePicker.Date := EncodeDate(2002, 1, 1);
  GotoDatePickerChange(nil);
  TimeIncComboChange(nil);
end;

procedure TPhotoOpMain.PrintButtonClick(Sender: TObject);
begin
  With JvTFDaysPrinter1 do
    Begin
      // "Copy" the display properties from the JvTFDays control
      SetProperties(JvTFDays1);
      // Set gridline color to black for sharp display on printed page
      GridLineColor := clBlack;
      // print 48 rows on each page
      PageLayout.RowsPerPage := 48;
      // fit all the columns onto one page wide
      PageLayout.ColsPerPage := 0;
      // "Copy" the schedules from the JvTFDays control
      Cols.Assign(JvTFDays1.Cols);
      PrintProgress.Show;
      Application.ProcessMessages;
      // print the document
      PrintDirect;
      PrintProgress.Close;
    End;
end;

procedure TPhotoOpMain.JvTFDaysPrinter1ApptProgress(Sender: TObject;
  Current, Total: Integer);
begin
  If Current > Total Then
    Total := Current;
  PrintProgress.Label2.Caption := 'Processing appointment ' + IntToStr(Current) +
                                  ' of ' + IntToStr(Total) + ' (estimated)';
  PrintProgress.ProgressBar1.Max := Total;
  PrintProgress.ProgressBar1.Position := Current;
end;

procedure TPhotoOpMain.JvTFDaysPrinter1AssembleProgress(Sender: TObject;
  Current, Total: Integer);
begin
  PrintProgress.Label2.Caption := 'Assembling page ' + IntToStr(Current) +
                                  ' of ' + IntToStr(Total); 
  PrintProgress.ProgressBar1.Max := Total;
  PrintProgress.ProgressBar1.Position := Current;
end;

procedure TPhotoOpMain.JvTFDaysPrinter1PrintProgress(Sender: TObject;
  Current, Total: Integer);
begin
  PrintProgress.Label2.Caption := 'Printing page ' + IntToStr(Current) +
                                  ' of ' + IntToStr(Total);
  PrintProgress.ProgressBar1.Max := Total;
  PrintProgress.ProgressBar1.Position := Current;
end;

procedure TPhotoOpMain.utfScheduleManager1LoadBatch(Sender: TObject;
  BatchName: String; BatchStartDate, BatchEndDate: TDate);
var
  Appt : TJvTFAppt;
  NewAppt : Boolean;
begin
  With NeedApptsQuery do
    Begin
      // Set the query parameters so the query will return
      // all appointments for the given resource that fall
      // on the given date.
      ParamByName('D1').AsDate := BatchStartDate;
      ParamByName('D2').AsDate := BatchEndDate;
      ParamByName('SchedName').AsString := BatchName;

      // Next, loop through the returned records to add the data
      Open;
      First;
      While not EOF do
        Begin
          // Request an appointment object from the server
          utfScheduleManager1.RequestAppt(FieldByName('ApptID').AsString,
            Appt, NewAppt);

          // If it is a newly loaded appt we want to set its properties
          If NewAppt Then
            Begin
              Appt.SetStartEnd(FieldByName('StartDate').AsDateTime,
                               FieldByName('StartTime').AsDateTime,
                               FieldByName('EndDate').AsDateTime,
                               FieldByName('EndTime').AsDateTime);
              Appt.Description := FieldByName('Description').AsString;
              Appt.AlarmEnabled := FieldByName('AlarmEnabled').AsBoolean;
              Appt.AlarmAdvance := FieldByName('AlarmAdvance').AsInteger;

              // Now manage the Appt --> Schedule(s) relationship
              With ApptSchedulesQuery do
                Begin
                  ParamByName('ApptID').AsString := Appt.ID;
                  Open;
                  First;
                  While not EOF do
                    Begin
                      Appt.AddSchedule(FieldByName('SchedName').AsString);
                      Next;
                    End;
                  Close; // ApptSchedulesQuery
                End;
            End;
          Next; // NeedApptsQuery record
        End;
      Close;  // NeedApptsQuery
    End;
end;

procedure TPhotoOpMain.FormCreate(Sender: TObject);
var
   DataPath: string;
begin
   DataPath := '..\examples\JvTimeFramework\PhotoOp\Data\';
   dbUTF.Params.Add('PATH=' + DataPath);
end;

end.