program StartRestore;

{$APPTYPE CONSOLE}

uses
  SysUtils, JvUIBLib, JvUIBase;

const
  user = 'SYSDBA';
  pass = 'masterkey';

var
  spb, thd, respbuf: string;
  svc_handle: IscSvcHandle;
  Len: Word;
  FLibrary: TUIBLibrary = nil;
begin

  if (ParamCount <> 2) then
  begin
    WriteLn(format('Usage: %s backupfile dbfile', [ExtractFileName(ParamStr(0))]));
    Readln;
    Halt(1);
  end;
  FLibrary := TUIBLibrary.Create;
  try
    spb := isc_spb_version + isc_spb_current_version;

    spb := spb + isc_spb_user_name;
    spb := spb + Char(Length(user));
    spb := spb + user;

    spb := spb + isc_spb_password;
    spb := spb + Char(Length(pass));
    spb := spb + pass;

    FLibrary.ServiceAttach('service_mgr', svc_handle, spb);
    try
      thd := isc_action_svc_restore;

      thd := thd + isc_spb_bkp_file;
      thd := thd + Char(Length(ParamStr(1)));
      thd := thd + Char(Length(ParamStr(1)) shr 8);
      thd := thd + ParamStr(1);

      thd := thd + isc_spb_dbname;
      thd := thd + Char(Length(ParamStr(2)));
      thd := thd + Char(Length(ParamStr(2)) shr 8);
      thd := thd + ParamStr(2);

      thd := thd + isc_spb_verbose;
      thd := thd + isc_spb_options;
      thd := thd +
        Char(isc_spb_res_replace) +
        Char(isc_spb_res_replace shr 8) +
        Char(isc_spb_res_replace shr 16) +
        Char(isc_spb_res_replace shr 32);

      FLibrary.ServiceStart(svc_handle, thd);

      SetLength(respbuf, 1024);

      while true do
      begin
        FLibrary.ServiceQuery(svc_handle, '', isc_info_svc_line, respbuf);
        if (respbuf[1] <> isc_info_svc_line) then
        begin
          WriteLn('Invalid line.');
          Exit;
        end;
        Len := PWord(@respbuf[2])^;
        if len > 0 then
          Writeln(copy(respbuf, 4, len)) else
          Break;
      end;
    finally
      FLibrary.ServiceDetach(svc_handle);
      writeln('Press enter to continue.');
      Readln;
    end;
  finally
    FLibrary.Free;
  end;

end.

