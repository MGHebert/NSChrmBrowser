unit FrameWB;

{.$DEFINE TESTMODE}


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, ExtCtrls,
  Forms, Dialogs, strUtils, LogFile, OtherLanguages,
  NSDatSettingsUtils, Registry, Constants, ceflib, cefvcl;

type
  TWBFrame = class(TFrame)
    Chrm: TChromium;
    procedure ChrmAddressChange(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; const url: ustring);
    //procedure ChrmBeforeMenu(Sender: TObject; const browser: ICefBrowser;
    //  const menuInfo: PCefMenuInfo; out Result: Boolean);
    procedure ChrmGetDownloadHandler(Sender: TObject;
      const browser: ICefBrowser; const mimeType, fileName: ustring;
      contentLength: Int64; var handler: ICefDownloadHandler;
      out Result: Boolean);
    procedure ChrmLoadEnd(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; httpStatusCode: Integer);
    procedure ChrmStatusMessage(Sender: TObject; const browser: ICefBrowser;
      const value: ustring; out Result: Boolean);
    procedure ChrmLoadError(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; errorCode: Integer; const errorText,
      failedUrl: ustring);
    procedure ChrmBeforeBrowse(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; const request: ICefRequest;
      isRedirect: Boolean; out Result: Boolean);
    procedure ChrmLoadStart(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame);
    procedure ChrmBeforePopup(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean; out Result: Boolean);
    procedure ChrmBeforeDownload(Sender: TObject; const browser: ICefBrowser;
      const downloadItem: ICefDownloadItem; const suggestedName: ustring;
      const callback: ICefBeforeDownloadCallback);
    procedure ChrmDownloadUpdated(Sender: TObject; const browser: ICefBrowser;
      const downloadItem: ICefDownloadItem;
      const callback: ICefDownloadItemCallback);
    procedure ChrmCertificateError(Sender: TObject; certError: Integer;
      const requestUrl: ustring;
      const callback: ICefAllowCertificateErrorCallback; out Result: Boolean);
    procedure ChrmProcessMessageReceived(Sender: TObject;
      const browser: ICefBrowser; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
    procedure ChrmContextMenuCommand(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer;
      eventFlags: TCefEventFlags; out Result: Boolean);
  private
    { Private declarations }
    FLoading: Boolean;
    function IsMain(const b: ICefBrowser; const f: ICefFrame = nil): Boolean;
    function AddDNSToRegKey(DNS:String) : String;
  public
    { Public declarations }
  end;

  TCefStreamDownloadHandler = class(TCefDownloadHandlerOwn)
    FStream: TStream;
    FOwned: Boolean;
  private
  protected
    //function ReceivedData(data: Pointer; DataSize: Integer): Integer; override;
    //procedure Complete; override;
  public
      procedure OnDownloadUpdated(const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
        const callback: ICefDownloadItemCallback); virtual;

    constructor Create(stream: TStream; Owned: Boolean); reintroduce;
    destructor Destroy; override;
  end;

var
  sCurrentURL : string;

implementation

uses uFormDCEWebBrowser, Utilities, HostVerify;

{$R *.dfm}

function TWBFrame.AddDNSToRegKey(DNS: String): String;
var
  DnsEntry : String;
begin
  DNSEntry := '*' + DNS + '*';
  FormDCEWebBrowser.StringListRestrictedURLs.Add(DNSEntry);
  Result := DNSEntry;
end;

procedure TWBFrame.ChrmAddressChange(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin

 if IsMain(browser, frame) then
   FormDCEWebBrowser.ComboBoxLocation.Text := FixFileName(url);

  // Check if page is https secure, if so show the security icon
  if pos('https:',FormDCEWebBrowser.ComboBoxLocation.Text) > 0 then
    FormDCEWebBrowser.StatusBarWebBrowser.Panels[1].Text := 'HTTPS'
  else
    FormDCEWebBrowser.StatusBarWebBrowser.Panels[1].Text := '';

  FormDCEWebBrowser.rkSmartTabs.Tabs.Strings[FormDCEWebBrowser.rkSmartTabs.ActiveTab] := FormDCEWebBrowser.GetTabName(url);
  FormDCEWebBrowser.rkSmartTabs.Refresh;
end;


procedure TWBFrame.ChrmBeforeBrowse(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; const request: ICefRequest;
  isRedirect: Boolean; out Result: Boolean);
var
  vURLString: String;
  vIndex: Integer;
  vFound: Boolean;
  vFileName: String;
  vEmailEnabled: Boolean;
  vTmpDefDir : string;
  i : integer;
  vList : string;
  fWebBrowserRestrictionMode : integer;
begin
  //mh 1/11/2014
  Application.ProcessMessages;

  fWebBrowserRestrictionMode :=  FormDCEWebBrowser.fWebBrowserRestrictionMode;

  if FormDCEWebBrowser.iOtherAppIndex = 0 then
  begin
    if NS_ReadBool('Web Browser - Load URL Restrictions From File') then
    begin
      //mh 1/11/2014
      //fWebBrowserRestrictionMode := 0;
      vList := NS_ReadString('Web Browser - URL Restrictions Filename');
      Application.ProcessMessages;
      if (vList <> '') and (FileExists(vList)) then
      begin
        if FormDCEWebBrowser.TextFileIsUnicode(vList) then
          FormDCEWebBrowser.LoadUnicodeFile( vList, FormDCEWebBrowser.StringListRestrictedURLs)
        else
          FormDCEWebBrowser.LoadTextFile( vList, FormDCEWebBrowser.StringListRestrictedURLs);
      end;
    end;
  end else begin
    if NS_ReadBool('Other Applications ' + IntToStr(FormDCEWebBrowser.iOtherAppIndex) + ' - Load URL Restrictions From File') then
    begin
      vList := NS_ReadString('Other Applications ' + IntToStr(FormDCEWebBrowser.iOtherAppIndex) + ' - URL Restrictions Filename');
      Application.ProcessMessages;
      if (vList <> '') and (FileExists(vList)) then
      begin
        if FormDCEWebBrowser.TextFileIsUnicode(vList) then
          FormDCEWebBrowser.LoadUnicodeFile( vList, FormDCEWebBrowser.StringListRestrictedURLs)
        else
          FormDCEWebBrowser.LoadTextFile( vList, FormDCEWebBrowser.StringListRestrictedURLs);
      end;
    end;
  end;

  //if FormDCEWebBrowser.StringListRestrictedURLs.Count > 0 then
  //  fWebBrowserRestrictionMode := 0;


  vURLString := Request.Url;
  vURLString := StringReplace(vURLString,' ','%20',[rfReplaceAll]);
  //temp
  //ShellProLog.DLog('ChrmBeforeBrowse - URL: ' + vUrlString);

  //mh 4/4/2012
  //mh 7/19/2015 new behavior?
  {
  if (RightStr(LowerCase(vURLString),4) = '.pdf') and
     (pos('#toolbar',vURLString) = 0) and
     (pos('yahoo',vURLString) = 0) and
     (pos('mail',vURLString) = 0) and
     (pos('exchange',vURLString) = 0) then
  begin
    vURLString := vURLString + '#toolbar=0';

    if FormDCEWebBrowser.ComboBoxLocation.Items.Count = 0 then
      Exit;

    if FormDCEWebBrowser.ComboBoxLocation.Items.Count > 0 then
    begin
      Result := False;
      Browser.StopLoad;
      //i := high(FormDCEWebBrowser.aCrmPanel);
      //if FormDCEWebBrowser.aCrmPanel[i] <> nil then
      //  FormDCEWebBrowser.aCrmPanel[i].Visible := False;
      //FormDCEWebBrowser.rkSmartTAbs.Tabs.Add(FormDCEWebBrowser.GetTabName(vUrlString));


      FormDCEWebBrowser.OpenNewTab(vUrlString);
      Exit;
    end;
  end;
  }

  // We need a copy of the URL as a String type.
  vURLString := Trim(LowerCase(vURLString));
  sCurrentURL := vURLString;

  vTmpDefDir := StringReplace(FormDCEWebBrowser.GetTempDirectory,' ','%20',[rfReplaceAll]);

  // Squawk if Local Drive is being accessed when it is set not to be
  if (Copy(vURLString, 2, 1) = ':') and
      not (FormDCEWebBrowser.fWebBrowserAllowLocalDriveAccess) and
      not (pos(vTmpDefDir, sCurrentURL) > 0) then
  begin
    ShellProLog.DLog('ChrmLoadStart - Local Drive Access attempt! - Exiting.');
    FormDCEWebBrowser.DisplayFNMessageForm(GetTextFromResourceOrRegistry(FormDCEWebBrowser.fKioskDefaultLanguage, 380), 4, 'Error', 5000);
    Browser.StopLoad;
    Exit;
  end;

  if (FormDCEWebBrowser.fWebBrowserRestrictPrintingMode <> 2) and (vURLString <> 'about:blank') then
  begin
    FormDCEWebBrowser.IPCSynchFromShellPro('SetAllowToPrintFalse');
    for vIndex := 0 to FormDCEWebBrowser.StringListAllowToPrintURLs.Count - 1 do
      if MatchStrings(vURLString, FormDCEWebBrowser.StringListAllowToPrintURLs.Strings[vIndex]) then
      begin
        if FormDCEWebBrowser.fWebBrowserRestrictionMode = 1 then
          FormDCEWebBrowser.IPCSynchFromShellPro('SetAllowToPrintTrue');
        Break;
      end;
  end;

  // Check if a blank URL was passed. In this case the navigation is cancelled.
  if (Length(trim(vURLString)) < 5) or (Length(trim(GetWebAddressServer(vURLString))) = 0) then
  begin
    ShellProLog.DLog('ChrmLoadStart - Blank URL Passed! - Exiting.');
    Browser.StopLoad;
    Exit;
  end;

  // Check if \\ is used to access local resources.
  if (Copy(vURLString, 1, 2) = '\\') then
  begin
    ShellProLog.DLog('ChrmBeforeBrowse - \\ used to access local resource! - Exiting.');
    FormDCEWebBrowser.DisplayFNMessageForm(GetTextFromResourceOrRegistry(FormDCEWebBrowser.fKioskDefaultLanguage, 784), 4, 'Error', 5000);
    Browser.StopLoad;
    Exit;
  end;

  // Check if the navigation is restricted. If it is, the navigation is not
  // allowed unless the URL to which it is navigating is in the list.
  // Email links are allowed.
  if (Pos('mailto:', vURLString) = 0) and FormDCEWebBrowser.fCurrentWebBrowserNavigationRestricted and IsHTTP(vURLString) then
  begin
      //mh temp
      //ShellProLog.DLog('-----------------------------------------------------------');
      //ShellProLog.DLog(' URL:' + vURLString);
      //ShellProLog.DLog('Text:' + FormDCEWebBrowser.StringListRestrictedURLs.Text);
      //ShellProLog.DLog('1 ChrmLoadStart - Navigation restricted! - Exiting.');
      //ShellProLog.DLog('-----------------------------------------------------------');
      //
    vIndex := 0;
    vFound := False;
    while (not vFound) and (vIndex < FormDCEWebBrowser.StringListRestrictedURLsHTMLBannersAndOther.Count)  do
    begin
       vFound := MatchStrings(vURLString, FormDCEWebBrowser.StringListRestrictedURLsHTMLBannersAndOther.Strings[vIndex]);
       Inc(vIndex);
    end;
    //mh 1/14/2014
    if (not vFound) and (FormDCEWebBrowser.StringListRestrictedURLsHTMLBannersAndOther.Count > 0)then
    begin
      //mh temp
      ShellProLog.DLog('-----------------------------------------------------------');
      ShellProLog.DLog('1');
      ShellProLog.DLog(' URL:' + vURLString);
      ShellProLog.DLog('Text:' + FormDCEWebBrowser.StringListRestrictedURLs.Text);
      ShellProLog.DLog('ChrmLoadStart - Navigation restricted! - Exiting.');
      ShellProLog.DLog('-----------------------------------------------------------');

      {$IFNDEF TESTMODE}
      FormDCEWebBrowser.DisplayFNMessageForm(GetTextFromResourceOrRegistry(FormDCEWebBrowser.fKioskLanguage, 378), 4, 'Error', 5000);
      Browser.StopLoad;
      exit;
      {$ENDIF}
    end;
  end;

  // Check URL restrictions, if it applies (only add main document (not frames)).
  // if pDisp = WebBrowser.Application then
  //if Frame.IsMain then
  //begin
  //  case FormDCEWebBrowser.fWebBrowserRestrictionMode of
    case fWebBrowserRestrictionMode of
      0: begin
           vIndex := 0;
           vFound := False;
           while (not vFound) and (vIndex < FormDCEWebBrowser.StringListRestrictedURLs.Count)  do
           begin
             Sleep(0);
             vFound := MatchStrings(vURLString, FormDCEWebBrowser.StringListRestrictedURLs.Strings[vIndex]);
             Inc(vIndex);
           end;
           //Check if the url in the allow list of IP Address
           if (not vFound) and FileExists('ValidIPsText.txt') then
           begin
             vFound := frmHostVerify.IsHostValid(vURLString);
             if VFound then
               AddDNSToRegKey(frmHostVerify.StripHost(vURLString));
           end;

           if not vFound then
           begin
             ShellProLog.DLog('0-ChrmLoadStart - Site Not Allowed! - Exiting.');
             {$IFNDEF TESTMODE}
             //mh temp
             ShellProLog.DLog('-----------------------------------------------------------');
             ShellProLog.DLog('2');
             ShellProLog.DLog(' URL:' + vURLString);
             ShellProLog.DLog('Text:' + FormDCEWebBrowser.StringListRestrictedURLs.Text);
             ShellProLog.DLog('ChrmLoadStart - Navigation restricted! - Exiting.');
             ShellProLog.DLog('-----------------------------------------------------------');
             //
             FormDCEWebBrowser.DisplayFNMessageForm(GetTextFromResourceOrRegistry(FormDCEWebBrowser.fKioskLanguage, 378), 4, 'Error', 5000);
             Browser.StopLoad;
             {$ENDIF}
           end;
         end;
      1: for vIndex := 0 to FormDCEWebBrowser.StringListRestrictedURLs.Count - 1 do
         begin
           if MatchStrings(vURLString, FormDCEWebBrowser.StringListRestrictedURLs.Strings[vIndex]) then
           begin
             ShellProLog.DLog('1-ChrmLoadStart - Site Not Allowed! - Exiting.');

             {$IFNDEF TESTMODE}
             //mh temp
             ShellProLog.DLog('-----------------------------------------------------------');
             ShellProLog.DLog('3');
             ShellProLog.DLog(' URL:' + vURLString);
             ShellProLog.DLog('Text:' + FormDCEWebBrowser.StringListRestrictedURLs.Text);
             ShellProLog.DLog('ChrmLoadStart - Navigation restricted! - Exiting.');
             ShellProLog.DLog('-----------------------------------------------------------');
             //
             FormDCEWebBrowser.DisplayFNMessageForm(GetTextFromResourceOrRegistry(FormDCEWebBrowser.fKioskLanguage, 378), 4, 'Error', 5000);
             Browser.StopLoad;
             Exit;
             {$ENDIF}

           end;
         end;
    end;
  //end;                         // end (4.8.2)

  // Check if the Mail Form should be invoked.
  if (Pos('mailto:', vURLString) <> 0) then
  begin

    {$IFDEF DATSETTINGS}
    vEmailEnabled := NS_ReadBool('Email Send - Enabled');
    {$ELSE}
    with TRegistry.Create(KEY_READ) do          // new (4.8.2)
    try
      RootKey := MyRootKey;

      if OpenKey(NETSTOP_REG_KEY, FALSE) then
      begin
        vEmailEnabled := ReadBool('Email Send - Enabled');
        CloseKey;
      end;
    finally
      free;
    end;
    {$ENDIF}


    if (vEmailEnabled = False) then  // if NetStop Clients choose not to display the e-mail button/not to let the kiosk users to e-mail using the kiosk
      begin                          // e-mail meaning e-mailing to a tech support address from a web site (ex: suffing bellsouth.com and want to send comments or complaints)
        Browser.StopLoad;
        FormDCEWebBrowser.DisplayFNMessageForm(GetTextFromResourceOrRegistry(FormDCEWebBrowser.fKioskDefaultLanguage, 444), 0, 'Information', 5000);
      end
    else
      begin
        Application.ProcessMessages;
        {
        with TFormNewMessage.Create(self) do
        begin
          // Set the recipient and the subject on the New Message form using URL value.
          if Pos('?', vURLString) > 0 then
          begin
            EditRecipientsTo.Text := Copy(vURLString, Pos('mailto:', vURLString) + 7, Pos('?', vURLString) - 8);
            EditSubject.Text := ToNormalChars(Copy(vURLString, Pos('?', vURLString) + 1, 1000));
          end
          else
            EditRecipientsTo.Text := Copy(vURLString, Pos('mailto:', vURLString) + 7, 1000);

          Width := min(600, (Screen.Width * 6) div 10);
          Height := min(480, (Screen.Height * 6) div 10);
          Left := ((Screen.Width - Width) div 2);
          Top := (Screen.Height - Height) div 2;

          // If it is running in restricted mode then email links are still allowed
          // but the user cannot change the destination.
          EditRecipientsTo.Enabled := not FormDCEWebBrowser.fCurrentWebBrowserNavigationRestricted;
          EditRecipientsCC.Enabled := not FormDCEWebBrowser.fCurrentWebBrowserNavigationRestricted;

          // Show New Message form.
          Show;
        end;
        }
        Browser.StopLoad;
        Exit;
      end;
  end;

  // Check if the NewsGroups Form should be invoked - if so cancel navigation.
  if Copy(vURLString, 1, 5) = 'news:' then
  begin
    Browser.StopLoad;
    Exit;
  end;

  // Check if the Telnet Form should be invoked - if so cancel navigation.
  if Copy(vURLString, 1, 7) = 'telnet:' then
  begin
    Browser.StopLoad;
    Exit;
  end;

  // Check if an FTP link was clicked.
  if (Copy(vURLString, 1, 4) = 'ftp:')
  and ( not(FormDCEWebBrowser.fEnableFTP) )  then
  begin
    FormDCEWebBrowser.DisplayFNMessageForm(GetTextFromResourceOrRegistry(FormDCEWebBrowser.fKioskLanguage, 174), 3, 'Error', 3500);
    Browser.StopLoad;
    Exit;
  end;

  // Remove parameters.
  if  Pos('?', vURLString) > 0 then
    vURLString := Copy(vURLString, 1, Pos('?', vURLString) - 1);

  // Check if the user is trying to access a file located on a drive he does
  // not have access to. This only applies if ComboBoxLocation is visible to
  // allow free links to be stored on local drives.
  if (FormDCEWebBrowser.ComboBoxLocation.Visible and
     ((Copy(vURLString, 1, 5) = 'file:') or (vURLString[2] = ':'))) then
  begin
    // Clean up file name.
    if (Copy(vURLString, 1, 5) = 'file:') then
      vURLString := Copy(vURLString, 6, 1000);

    while Length(vURLString) > 0 do
    begin
      if (vURLString[1] = '/') then
        Delete(vURLString, 1, 1)
      else
        BREAK;
    end;

    if Pos('#', vURLString) > 0 then
      vURLString := Copy(vURLString, 1, Pos('#', vURLString) - 1);

    vFileName := ExpandFileName(vURLString);
    vFileName := StringReplace(vFileName,'%20',' ',[rfReplaceAll]);
    vFileName := StringReplace(vFileName,'%5B','[',[rfReplaceAll]);
    vFileName := StringReplace(vFileName,'%5D',']',[rfReplaceAll]);

    if (Pos(UpperCase(Copy(ExtractFileDrive(vFileName), 1, 1)), UpperCase(FormDCEWebBrowser.fAccessibleDrives)) <> 0) or
       (UpperCase(Copy(ExtractFileDrive(vFileName), 1, 1)) = UpperCase(FormDCEWebBrowser.fTempDirectoryVirtualDrive)) then
    begin
      // File is on available/accessible drive. Check if it exists.
      if not FileExists(vFileName) then
      begin
        FormDCEWebBrowser.DisplayFNMessageForm(GetTextFromResourceOrRegistry(FormDCEWebBrowser.fKioskLanguage, 379), 3, 'Error', 3500);
        Browser.StopLoad;
        Exit;
      end;
    end
    else
    begin
      //Allow if in user temp folder OR
      if (pos(vTmpDefDir, sCurrentURL) = 0) then
      begin
        FormDCEWebBrowser.DisplayFNMessageForm(GetTextFromResourceOrRegistry(FormDCEWebBrowser.fKioskLanguage, 380), 4, 'Error', 3500);
        Browser.StopLoad;
        Exit;
      end;
    end;
  end;

  //todo
  {
  // Check if there is special timer speed for this URL.
  //if pDisp = WebBrowser.Application then
  if Frame.IsMain then
  begin
    vFound := False;
    for vIndex := 0 to FormMainMenu.StringListTimerSpeedWebBrowser.Count - 1 do
    begin
      if MatchStrings(vURLString, Copy(FormMainMenu.StringListTimerSpeedWebBrowser.Strings[vIndex], Pos('^', FormMainMenu.StringListTimerSpeedWebBrowser.Strings[vIndex]) + 1, Length(FormMainMenu.StringListTimerSpeedWebBrowser.Strings[vIndex]) - Pos('^', FormMainMenu.StringListTimerSpeedWebBrowser.Strings[vIndex]))) then
      begin
        try
          FormMainMenu.SetCurrentTimerSpeed(StrToInt(Copy(FormMainMenu.StringListTimerSpeedWebBrowser.Strings[vIndex], 1, Pos('^', FormMainMenu.StringListTimerSpeedWebBrowser.Strings[vIndex]) - 1)));

         vFound := True;
        except
        end;
      end;
    end;
    if not vFound then
    begin
      FormMainMenu.SetCurrentTimerSpeed(100);
    end;
  end;
  }
end;


procedure TWBFrame.ChrmBeforeDownload(Sender: TObject;
  const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
  const suggestedName: ustring; const callback: ICefBeforeDownloadCallback);
begin
  //callback.Cont(ExtractFilePath(ParamStr(0)) + suggestedName, False);
  callback.Cont(sDefDir + suggestedName, False);
  with FormDCEWebBrowser do
  begin
    Screen.Cursor := crHourglass;
    ProgressBar.Visible := True;
    ProgressBar.Max := 100;
    //sCurrentFileSpec := sDefDir + filename;
    sCurrentFileSpec := sDefDir + suggestedName;
    SetCurrentDir(sDefDir);

    TimerFileDialog.Enabled := True;
  end;

end;

//procedure TWBFrame.ChrmBeforeMenu(Sender: TObject; const browser: ICefBrowser;
//  const menuInfo: PCefMenuInfo; out Result: Boolean);
//begin
//  // True to disable Right Click
//  //Result := True;
//  Result := (not FormDCEWebBrowser.fWebBrowserRightClick);
//end;


procedure TWBFrame.ChrmBeforePopup(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
  var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var client: ICefClient; var settings: TCefBrowserSettings;
  var noJavascriptAccess: Boolean; out Result: Boolean);
begin
  // True here disables popups (PDFs opened in a popup...)
  // Google docs requires popups - no found yet to divert to another tab

  ShellProLog.DLog('ChrmBeforePopup - vURL: ' + targetUrl);

  if (pos('.pdf',LowerCase(targetUrl)) > 0) and
     (pos('jetblue',LowerCase(targetUrl)) = 0) then
  begin
    FormDCEWebBrowser.ComboBoxLocation.Text := targetUrl;
    FormDCEWebBrowser.NavigateToComboBoxValue(nil);
    Result := True;
  end else begin
    Chrm.Load(targetUrl);
    Result := True;
  end;
end;


procedure TWBFrame.ChrmDownloadUpdated(Sender: TObject;
  const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
begin
 if downloadItem.IsInProgress then
    FormDCEWebBrowser.StatusBarWebBrowser.SimpleText := IntToStr(downloadItem.PercentComplete) + '%' else
    FormDCEWebBrowser.StatusBarWebBrowser.SimpleText := '';

end;

procedure TWBFrame.ChrmGetDownloadHandler(Sender: TObject;
  const browser: ICefBrowser; const mimeType, fileName: ustring;
  contentLength: Int64; var handler: ICefDownloadHandler; out Result: Boolean);
begin
  with FormDCEWebBrowser do
  begin
    Screen.Cursor := crHourglass;
    ProgressBar.Visible := True;
    ProgressBar.Max := 100;
    sCurrentFileSpec := sDefDir + filename;
    handler := TCefStreamDownloadHandler.Create(TFileStream.Create(sCurrentFileSpec, fmCreate), true);
    Result := True;
  end;
end;


procedure TWBFrame.ChrmLoadEnd(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer);
var
  vCounter: Integer;
  vFound: Boolean;
  vURL: String;
begin
  if IsMain(browser, frame) then
    FLoading := False;

  //mh 08/19/2009 this turns off for some reason sporadically
  ShowCursor(True);

  // Only add main document (not frames) to the list.
  if Frame.IsMain then
  begin
    // Add the URL to the list only if it has not been added yet.

    if Pos(Frame.Url, '?') > 0 then
      vURL := Copy(Frame.Url, 1, min(Pos(Frame.Url, '?') - 1, 512))
    else
      vURL := Copy(Frame.Url, 1, 512);

    vCounter := 0;
    vFound := False;
    try
      while (not vFound) and (vCounter < FormDCEWebBrowser.ComboBoxLocation.Items.Count) do
      begin
        vFound :=  Uppercase(FormDCEWebBrowser.ComboBoxLocation.Items[vCounter]) = Uppercase(vURL);
        Inc(vCounter);
      end;
    except
      ShellProLog.DLog('ERR - ChrmLoadEnd - ComboBoxLocation Error - 1');
    end;

    if not vFound then
    begin
      FormDCEWebBrowser.ComboBoxLocation.Items.Insert(0, vURL);
      try
        if ShellProLog.RecordSitesVisited then
          ShellProLog.CreateLogEntry('WSV', vURL, False);
      except
        ShellProLog.DLog('ERR - ChrmLoadEnd - ComboBoxLocation Error - 2');
      end;
    end;

    try
      //mh 5/5/2010
      if (FormDCEWebBrowser.ComboBoxLocation.Enabled) and (FormDCEWebBrowser.ComboBoxLocation.Visible) then
        FormDCEWebBrowser.ComboBoxLocation.Text := Frame.URL;
    except
      ShellProLog.DLog('ERR - ChrmLoadEnd - ComboBoxLocation Error - 3');
    end;
  end;

  with FormDCEWebBrowser do
  begin

    if (pos('file:',vURL) > 0) and
       (pos('#toolbar',vURL) > 0) then
      FormDCEWebBrowser.rkSmartTabs.Tabs.Strings[rkSmartTabs.ActiveTab] := copy(vURL,9,length(ComboBoxLocation.Text) - 9);

    if rkSmartTabs.Tabs.Strings[rkSmartTabs.ActiveTab] = '' then
    begin
      rkSmartTabs.Tabs.Strings[rkSmartTabs.ActiveTab] := FixFileName(ComboBoxLocation.Text);
    end;
    FormDCEWebBrowser.rkSmartTabs.Repaint;
  end;

  FormDCEWebBrowser.TimerAnimation.Enabled := False;

end;

procedure TWBFrame.ChrmLoadError(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; errorCode: Integer; const errorText,
  failedUrl: ustring);
begin
  ShellProlog.DLog('ChrmLoadError: ' + String(FailedUrl) + ' ' + IntToStr(errorCode) + ' ' +String(errorText));
end;

procedure TWBFrame.ChrmLoadStart(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame);
begin
  FormDCEWebBrowser.TimerAnimation.Enabled := True;
  if IsMain(browser, frame) then
    FLoading := True;
end;

procedure TWBFrame.ChrmStatusMessage(Sender: TObject;
  const browser: ICefBrowser; const value: ustring;
  out Result: Boolean);
begin
  FormDCEWebBrowser.StatusBarWebBrowser.Panels[0].Text := value;
end;

function TWBFrame.IsMain(const b: ICefBrowser; const f: ICefFrame): Boolean;
begin
  Result := (b <> nil) and (b.Identifier = Chrm.BrowserId) and ((f = nil) or (f.IsMain));
end;

{ TCefStreamDownloadHandler }
{
procedure TCefStreamDownloadHandler.Complete;
begin
  inherited;
  Screen.Cursor := crDefault;
  with FormDCEWebBrowser do
  begin
    ProgressBar.Position := 0;
    ProgressBar.Visible := False;
    StatusBarWebBrowser.Panels[0].Text := 'Download complete';
    TimerFileDialog.Enabled := True;

    // Check if page is https secure, if so show the security icon
    if pos('https:',ComboBoxLocation.Text) > 0 then
      FormDCEWebBrowser.StatusBarWebBrowser.Panels[1].Text := 'HTTPS'
    else
      FormDCEWebBrowser.StatusBarWebBrowser.Panels[1].Text := '';

    // Enable the Refresh button.
    ButtonRefresh.Enabled := True;

    // Enable the Open button.
    ButtonOpen.Enabled := True;

    // Enable the Print button.
    //mh 6/28/2012 hack to disable print button while viewing/editing docs on HotMail/Live Mail
    if (ComboBoxLocation.Text = 'about:blank') or
       (ComboBoxLocation.Text = 'javascript:' + chr(39) + chr(39)) or
       (pos('word-view',lowercase(ComboBoxLocation.Text)) > 0) or
       (pos('excel.officeapps',lowercase(ComboBoxLocation.Text)) > 0) or
       (pos('inboxnocompose',lowercase(ComboBoxLocation.Text)) > 0) then
    begin
      fPrintingEnabled := False;
      ButtonPrint.Enabled := False;
    end else begin
      fPrintingEnabled := True;
      ButtonPrint.Enabled := True;
    end;

    TimerAnimation.Enabled := False;

    // Disable the Stop button.
    ButtonStop.Enabled := False;
  end;

end;
}
constructor TCefStreamDownloadHandler.Create(stream: TStream; Owned: Boolean);
begin
  inherited Create;
  FStream := stream;
  FOwned := Owned;

  with FormDCEWebBrowser do
  begin
    // Disable the refresh button.
    ButtonRefresh.Enabled := False;

    // Disable the Open button.
    ButtonOpen.Enabled := False;

    // Disable the Print button.
    fPrintingEnabled := False;
    ButtonPrint.Enabled := False;

    // Enable the Stop button.
    ButtonStop.Enabled := True;

    TimerAnimation.Enabled := True;
  end;

end;

destructor TCefStreamDownloadHandler.Destroy;
begin
  if FOwned then
    FStream.Free;
  inherited;
end;


procedure TCefStreamDownloadHandler.OnDownloadUpdated(
  const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
begin
  inherited;
  Screen.Cursor := crDefault;
  with FormDCEWebBrowser do
  begin
    ProgressBar.Position := 0;
    ProgressBar.Visible := False;
    StatusBarWebBrowser.Panels[0].Text := 'Download complete';
    TimerFileDialog.Enabled := True;

    // Check if page is https secure, if so show the security icon
    if pos('https:',ComboBoxLocation.Text) > 0 then
      FormDCEWebBrowser.StatusBarWebBrowser.Panels[1].Text := 'HTTPS'
    else
      FormDCEWebBrowser.StatusBarWebBrowser.Panels[1].Text := '';

    // Enable the Refresh button.
    ButtonRefresh.Enabled := True;

    // Enable the Open button.
    ButtonOpen.Enabled := True;

    // Enable the Print button.
    //mh 6/28/2012 hack to disable print button while viewing/editing docs on HotMail/Live Mail
    if (ComboBoxLocation.Text = 'about:blank') or
       (ComboBoxLocation.Text = 'javascript:' + chr(39) + chr(39)) or
       (pos('word-view',lowercase(ComboBoxLocation.Text)) > 0) or
       (pos('excel.officeapps',lowercase(ComboBoxLocation.Text)) > 0) or
       (pos('inboxnocompose',lowercase(ComboBoxLocation.Text)) > 0) then
    begin
      fPrintingEnabled := False;
      ButtonPrint.Enabled := False;
    end else begin
      fPrintingEnabled := True;
      ButtonPrint.Enabled := True;
    end;

    TimerAnimation.Enabled := False;

    // Disable the Stop button.
    ButtonStop.Enabled := False;
  end;
end;

//function TCefStreamDownloadHandler.ReceivedData(data: Pointer;
//  DataSize: Integer): Integer;
//begin
//  Result := FStream.Write(data^, DataSize);
//  with FormDCEWebBrowser do
//  begin
//    StatusBarWebBrowser.Panels[0].Text := 'Downloading ... ' + IntToStr(FStream.Position div 1000) + ' Kb';
//    //hack here - total size not known - set max at 1mb then reset as needed
//    Progressbar.Position := (Progressbar.Position + FStream.Position) div 1000;
//  end;
//end;

procedure TWBFrame.ChrmCertificateError(Sender: TObject; certError: Integer;
  const requestUrl: ustring; const callback: ICefAllowCertificateErrorCallback;
  out Result: Boolean);
begin
  // let use untrusted certificates (ex: cacert.org)
  callback.Cont(True);
  Result := True;
end;

procedure TWBFrame.ChrmContextMenuCommand(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: TCefEventFlags; out Result: Boolean);
begin
  Result := False;
end;

procedure TWBFrame.ChrmProcessMessageReceived(Sender: TObject;
  const browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; out Result: Boolean);
begin
  if (message.Name = 'mouseover') then
  begin
    FormDCEWebBrowser.StatusBarWebBrowser.SimpleText := message.ArgumentList.GetString(0);
    Result := True;
  end else
    Result := False;

end;

end.
