unit JvDebugHandler; {JvDebugHandler.pas version 2.0.0.0 for use with Delphi versions 7 and 9(2005}

{ The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/NPL/NPL-1_1Final.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
  the specific language governing rights and limitations under the License.

 { Many thanks to David Rose, who turned my original template code into the first
   version of a component to serve this purpose, and from which this component
   has evolved, and to Brian Weith who solved a problem neither of us really
   understands <g>, as well as Brad White who was of invaluable help to me!  And
   finally to the members of the Jedi group who wrote the wonderful underlying code
   which this component depends upon.}

 { © Copyright 2004 Robert Meek dba Tangentals Design  [All Rights Reserved]

    707 Rear Maple St.
    Minersville, Pa. U.S.A.  17954

    Phone:  (570) 544-2631
    FAX:    (570) 544-6547
    Cell:   (570) 590-3879
    E-mail:  rmeek@ptd.net
    WWW:  www.TangentalsDesign.com

    All code, files, and/or data pertaining to and referenced by the above named
    program or unit, as well as any ancillary files or information packaged with them
    are soley owned by Robert Meek dba Tangentals Design, and provided for educational
    and non-profit use only unless specifically stated to the contrary above.  Any
    other use, and/or distribution is strictly prohibited unless signed permission is
    first provided by me.

   Use this program, code, and/or information at your own Risk!}

 { INSTRUCTIONS:

   This non-visual component has been tested under Delphi 7 and 9 and can be used
   ONLY in conjuction with the Jedi Library and it's included Debug handling routines.
   All laws, rules, and provisions provided for under the Mozilla public license
   apply, and though this component may be freely used in both freeware/shareware,
   opensource and commercial projects, some restrictions DO apply, and so you are
   urged to read the MPL (license.txt) and MPL FAQ documents available in the
   JCL-Help.chm file that comes included with the Jedi Code Library.  The latest
   version of the Jedi Code Library can be found and downloaded from:
   http://www.delphi-jedi.org/Jedi:CODELIBJCL

   To use this component:  Install as a "new component" into an old or new package.
   Please note:  The above is true ONLY if you wish to make use of this pas file as
   is and NOT if it is but one component contained in a Delphi Package!  Provided you
   have correctly installed the Jedi Library, drop the component onto the MainForm
   of a project and set the following properties in the object inspector or in your
   mainform's OnCreate method as code.

   ExceptionLogging = True will send exception info to logfile and/or any other viewer
                   as set in the "OnOtherDestination" event.  In other words, this
                   turns the component on and off.  Note:  This is NOT the same as
                   loading/unloading the component!  It provides a means by which an
                   option could be set at runtime that will activate it.  A good example
                   would be in the case of an unexplained error on a users machine,
                   The user could be instructed to turn this on via a menu item, then
                   send the resulting log file back to you for analysis.
   StackTrackingEnabled = True does just what it says, providing a full stack trace of
                   any exceptions including line numbers.
   UnHandledExceptions = True if you only want those exceptions NOT handled by the
                   application to be logged.
   LogToFile = True allows for a text-based log file to be created.
   LogFileName = '' can be set here or in the mainform's OnCreate with or without path.
                   If no name or path is given, any logfiles created will be provided
                   the name:  Application.Title + 'ERRORLOG.txt'  and will be placed in
                   the application's directory.
   AppendLogFile = True will append the exception information generated to the beginning
                   of any logfile for this project already in existance.  If one doesn't
                   yet exist, it will be created.  Each new exception logged will appear
                   above the last and seperated by two blank lines.
   OnOtherDestination event:  is the only event provided.  When assigned, by double-
                   clicking it in the object inspector, a procedure of this name will
                   be created for you in your mainform's unit.  Any code you write here
                   will be run immeadiatly upon any exception information being generated,
                   before and completely independant of the component's own logfile and
                   whether or not "CreateLogFile" is set to True or False.
                   From here you may access the "ExceptionSgtringList" which holds this
                   information and do with it as wanted.  You may for example, have the
                   "ExceptionStringList" saved to another logfile, or to another utility
                   application such as CodeSite.
                   Please note that thwe "ExceptionStringList" is created and freed
                   properly by the component itself...you need only access it if wanted.
                   Also note that even though "AppendLogFile" may be set to True, this
                   property ONLY applies to the component's own logfile.  When the
                   "ExceptionStringList" is accessed from within the "OnOtherDestination"
                   method, it is holding ONLY the current exception's information.  To
                   use this information in an appended form, it will be necessary to write
                   such code as necessary.  As an example:  You could, within this method,
                   create a second stringList of your own and assign the "ExceptionStringList's"
                   lines to it each time the event fires.  Finally, as an example of how
                   CodeSite users can easily make use of the exception information, the code
                   as written below and added to "OnOtherDestination" method is all that
                   is needed:
                        For CodeSite 2:
                        If CodeSite.Enabled = True Then
                        CodeSite.SendStringList('ERROR INFO', JclDebugHandler1.ExceptionStringList);

                        For CodeSite 3:
                        If CodeSite.Enabled = True Then
                        CodeSite.Send('ERROR INFO: ', JclDebugHandler1.ExceptionStringList);

                   Although it is NOT necessary to check if CodeSite is enabled or not here, I
                   choose do do so because if I disable it and still leave this code in place,
                   an exception will not occurr.}

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  SysUtils, Classes, Forms,
  JclDebug, JclHookExcept;

type
  TJvDebugHandler = Class(TComponent)
  Private
    FExceptionLogging: Boolean;
    FStackTrackingEnable: Boolean;
    FUnhandledExceptionsOnly: Boolean;
    FLogToFile: Boolean;
    FName: string;
    FAppendLogFile: Boolean;
    FIsLoaded: Boolean;

    FOnOtherDestination: TNotifyEvent;
    FOldExceptionHandler: TExceptionEvent;
    procedure SetUnhandled(Value: Boolean);
    procedure HandleUnKnownException(Sender: TObject; E: Exception);
    procedure SetStackTracking(Value: Boolean);
    procedure ExceptionNotifier(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
  protected
    procedure Loaded; override;
  public
    ExceptionStringList: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ExceptionLogging: Boolean read FExceptionLogging write FExceptionLogging default True;
    property StackTrackingEnable: Boolean read FStackTrackingEnable write SetStackTracking default True;
    property UnhandledExceptionsOnly: Boolean read FUnhandledExceptionsOnly write SetUnhandled default False;
    property LogToFile: Boolean read FLogToFile write FLogToFile default True;
    property LogFileName: string read FName write FName;
    property AppendLogFile: Boolean read FAppendLogfile write FAppendLogFile default True;
    property OnOtherDestination: TNotifyEvent read FOnOtherDestination write FOnOtherDestination;
  end;

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

procedure TJvDebugHandler.HandleUnKnownException(Sender: TObject; E: Exception);
begin
  ExceptionNotifier(E, ExceptAddr, False);
  if Assigned(FOldExceptionHandler) then
    FOldExceptionHandler(Sender, E);
end;

procedure TJvDebugHandler.SetUnhandled(Value: Boolean);
begin
  if FUnhandledExceptionsOnly <> Value then
  begin
    FUnhandledExceptionsOnly := Value;
    if FUnhandledExceptionsOnly then
    begin
      JclRemoveExceptNotifier(ExceptionNotifier);
      FOldExceptionHandler := Application.OnException;
      Application.OnException := HandleUnKnownException
    end
    else
    begin
      if Assigned(FOldExceptionHandler) then
      begin
        Application.OnException := FOldExceptionHandler;
        FOldExceptionHandler := nil;
        JclAddExceptNotifier(ExceptionNotifier);
      end;
    end;
  end;
end;

procedure TJvDebugHandler.SetStackTracking(Value: Boolean);
begin
  if Value <> FStackTrackingEnable Then
  begin
    FStackTrackingEnable := Value;
    if FStackTrackingEnable then
      JclStartExceptionTracking
    else
      JclStopExceptionTracking;
  end;
end;

constructor TJvDebugHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExceptionLogging := True;
  FStackTrackingEnable := True;
  FLogToFile := True;
  FAppendLogfile := True;
  Loaded;
end;

destructor TJvDebugHandler.Destroy;
begin
  JclStopExceptionTracking;
  JclRemoveExceptNotifier(ExceptionNotifier);
  JclUnhookExceptions;
  inherited Destroy;
end;

procedure TJvDebugHandler.ExceptionNotifier(ExceptObj: TObject; ExceptAddr: Pointer;
  OSException: Boolean);
var
  I: Integer;
  PreviousExceptionStringList: TStringList;
  FileName: string;
  UnitName: string;
  ProcedureName: string;
  Line: Integer;
  Loc: string;
begin
  FileName := '';
  UnitName := '';
  ProcedureName := '';
  Loc := '';
  if FExceptionLogging then
  begin
    ExceptionStringList := TStringList.Create;
    try
      if MapOfAddr(ExceptAddr, FileName, UnitName, ProcedureName, Line) then
        Loc := Format('in %s at %d in file %s', [ProcedureName, Line, FileName])
      else
        Loc := Format('at address %p', [ExceptAddr]);

      ExceptionStringList.Add(DateTimeToStr(now) + ' Exception ' +
        ExceptObj.ClassName + ' occured ' + Loc);
      if ExceptObj is Exception then
        ExceptionStringList.Add('Message: ' + Exception(ExceptObj).Message);

      if FStackTrackingEnable then
      begin
        ExceptionStringList.Add('Call stack: ');
        if JclLastExceptStackList <> nil Then
          JclLastExceptStackList.AddToStrings(ExceptionStringList);
      end;

      if Assigned(FOnOtherDestination) Then
        FOnOtherDestination(Self);

      if FLogToFile Then
      begin
        if FName = '' then
          FName := ExtractFilePath(Application.ExeName) + Application.Title + 'ERRORLOG.txt';
        if not FAppendLogfile Then
          ExceptionStringList.SaveToFile(FName)
        else
        begin
          if not FileExists(FName) then
            ExceptionStringList.SaveToFile(FName)
          else
          begin
            PreviousExceptionStringList := TStringList.Create;
            try
              PreviousExceptionStringList.LoadFromFile(FName);
              ExceptionStringList.Add('');
              ExceptionStringList.Add('');
              for I := 0 to PreviousExceptionStringList.Count - 1 do
                ExceptionStringList.Add(PreviousExceptionStringList[i]);
              ExceptionStringList.SaveToFile(FName);
            finally
              PreviousExceptionStringList.Free;
            end;
          end;
        end;
      end;
    finally
      ExceptionStringList.Free;
    end;
  end;
end;

procedure TJvDebugHandler.Loaded;
begin
  if not FIsLoaded Then
  begin
    FIsLoaded := True;
    inherited Loaded;
    if csDesigning in ComponentState then
      Exit;
    if JclHookExceptions then
    begin
      if not FUnhandledExceptionsOnly then
        JclAddExceptNotifier(ExceptionNotifier);
      if FStackTrackingEnable Then
        JclStartExceptionTracking
      else
        JclStopExceptionTracking;
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

