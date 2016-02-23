object WBFrame: TWBFrame
  Left = 0
  Top = 0
  Width = 393
  Height = 316
  TabOrder = 0
  object Chrm: TChromium
    Left = 0
    Top = 0
    Width = 393
    Height = 316
    Align = alClient
    DefaultUrl = 'about:blank'
    TabOrder = 0
    OnProcessMessageReceived = ChrmProcessMessageReceived
    OnLoadStart = ChrmLoadStart
    OnLoadEnd = ChrmLoadEnd
    OnLoadError = ChrmLoadError
    OnContextMenuCommand = ChrmContextMenuCommand
    OnAddressChange = ChrmAddressChange
    OnBeforeDownload = ChrmBeforeDownload
    OnDownloadUpdated = ChrmDownloadUpdated
    OnBeforePopup = ChrmBeforePopup
    OnBeforeBrowse = ChrmBeforeBrowse
    OnCertificateError = ChrmCertificateError
  end
end
