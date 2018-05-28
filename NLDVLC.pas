unit NLDVLC;

// Author: Dany
// Date: 2018-05-25

{ History
  2018-05-25: Original version.
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type TAudioStreamItem =
  record
    Id: integer;
    Name: string;
  end;
  TAudioStreamList = array of TAudioStreamItem;

function VLC_Create: boolean;
procedure VLC_Destroy;
function VLC_Playing: boolean;
function VLC_Play(File_: string; Panel_: TPanel): boolean;
procedure VLC_Stop;
procedure VLC_Set_Volume(Volume_: integer);
function VLC_Get_Volume: integer;
procedure VLC_Toggle_Mute;
procedure VLC_Set_Time(Time_: Int64);
function VLC_Get_Time: Int64;
function VLC_Get_Length: Int64;
procedure VLC_Set_Pause(on: boolean); // pause/resume
procedure VLC_Toggle_pause;
procedure VLC_Get_Audio_Track_Info(var List_: TAudioStreamList);
procedure VLC_Set_Audio_Track(Track_: integer);
procedure VLC_Set_Mute(on: boolean); // mute/demute
function VLC_Get_Width: integer;
function VLC_Get_Height: integer;
function VLC_Get_FPS: Single;
procedure VLC_Set_Rate(Rate_: Single);
function VLC_Get_Rate: Single;

implementation

type
  plibvlc_instance_t = type Pointer;
  plibvlc_media_player_t = type Pointer;
  plibvlc_media_t = type Pointer;

  Plibvlc_track_description = ^Tlibvlc_track_description;
  Tlibvlc_track_description =
    record
    id: integer;
    name: PAnsichar;
    next_: Plibvlc_track_description;
  end;

var
  libvlc_media_new_path: function(p_instance: Plibvlc_instance_t; path: PAnsiChar): Plibvlc_media_t; cdecl;
  libvlc_media_new_location: function(p_instance: plibvlc_instance_t; psz_mrl: PAnsiChar): Plibvlc_media_t; cdecl;
  libvlc_media_player_new_from_media: function(p_media: Plibvlc_media_t): Plibvlc_media_player_t; cdecl;
  libvlc_media_player_new: function(p_instance: plibvlc_instance_t): Plibvlc_media_player_t; cdecl;
  libvlc_media_player_set_media: procedure(p_media_player: Plibvlc_media_player_t; p_media: Plibvlc_media_t); cdecl;
  libvlc_media_player_set_hwnd: procedure(p_media_player: Plibvlc_media_player_t; drawable: Pointer); cdecl;
  libvlc_media_player_play: function(p_media_player: Plibvlc_media_player_t): integer; cdecl;
  libvlc_media_player_stop: procedure(p_media_player: Plibvlc_media_player_t); cdecl;
  libvlc_media_player_release: procedure(p_media_player: Plibvlc_media_player_t); cdecl;
  libvlc_media_player_is_playing: function(p_media_player: Plibvlc_media_player_t): Integer; cdecl;
  libvlc_media_release: procedure(p_media: Plibvlc_media_t); cdecl;
  libvlc_new: function(argc: Integer; argv: PAnsiChar): Plibvlc_instance_t; cdecl;
  libvlc_release: procedure(p_instance: Plibvlc_instance_t); cdecl;

  libvlc_media_player_get_time: function(p_media_player: Plibvlc_media_player_t): int64; cdecl;
  libvlc_media_player_set_time: procedure(p_media_player: Plibvlc_media_player_t; Time: int64); cdecl;
  libvlc_media_player_get_length: function(p_media_player: Plibvlc_media_player_t): int64; cdecl;
  libvlc_media_player_set_pause: procedure(p_media_player: Plibvlc_media_player_t; Pause: integer); cdecl;
  libvlc_media_player_pause: procedure(p_instance: Plibvlc_instance_t); cdecl;

  libvlc_audio_get_volume: function(p_media_player: Plibvlc_media_player_t): integer; cdecl;
  libvlc_audio_set_volume: procedure(p_media_player: Plibvlc_media_player_t; Volume: integer); cdecl;
  libvlc_audio_set_mute: procedure(p_media_player: Plibvlc_media_player_t; Mute: integer); cdecl;
  libvlc_audio_toggle_mute: procedure(p_media_player: Plibvlc_media_player_t); cdecl;
  libvlc_audio_get_track_count: function(p_media_player: Plibvlc_media_player_t): integer; cdecl;
  libvlc_audio_get_track: function(p_media_player: Plibvlc_media_player_t): integer; cdecl;
  libvlc_audio_set_track: procedure(p_media_player: Plibvlc_media_player_t; Track: integer); cdecl;
  libvlc_audio_get_track_description: function(p_media_player: Plibvlc_media_player_t): Plibvlc_track_description; cdecl;
  libvlc_track_description_list_release: procedure(p_media_player: Plibvlc_media_player_t); cdecl;

  libvlc_video_get_height: function(p_media_player: Plibvlc_media_player_t): integer; cdecl;
  libvlc_video_get_width: function(p_media_player: Plibvlc_media_player_t): integer; cdecl;

  libvlc_media_player_get_fps: function(p_media_player: Plibvlc_media_player_t): single; cdecl;
  libvlc_media_player_get_rate: function(p_media_player: Plibvlc_media_player_t): single; cdecl;
  libvlc_media_player_set_rate: procedure(p_media_player: Plibvlc_media_player_t; Rate: single); cdecl;

  libvlc_video_get_spu_count: function(p_media_player: Plibvlc_media_player_t): integer; cdecl;
  libvlc_video_get_spu: function(p_media_player: Plibvlc_media_player_t): integer; cdecl;
  libvlc_video_set_spu: function(p_media_player: Plibvlc_media_player_t; Spu: integer): integer; cdecl;

  libvlc_video_set_mouse_input: procedure(p_media_player: Plibvlc_media_player_t; on: LongWord); cdecl;
  libvlc_video_set_key_input: procedure(p_media_player: Plibvlc_media_player_t; on: LongWord); cdecl;

  vlcLib: integer;
  vlcInstance: plibvlc_instance_t;
  vlcMediaPlayer: plibvlc_media_player_t;

  vlcMediaPlayerInitialised: boolean;
  Playing: boolean;

// -----------------------------------------------------------------------------
// Read registry to get VLC installation path
// -----------------------------------------------------------------------------

function GetVLCLibPath: string;
var
  Handle: HKEY;
  RegType: Integer;
  DataSize: Cardinal;
  Key: PWideChar;
begin
  Result := '';
  Key := 'Software\VideoLAN\VLC';
  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, Key, 0, KEY_READ, Handle) = ERROR_SUCCESS then
  begin
    if RegQueryValueEx(Handle, 'InstallDir', nil, @RegType, nil, @DataSize) = ERROR_SUCCESS then
    begin
      SetLength(Result, DataSize);
      RegQueryValueEx(Handle, 'InstallDir', nil, @RegType, PByte(@Result[1]), @DataSize);
      Result[DataSize] := '\';
    end
    else Showmessage('Error on reading registry');
    RegCloseKey(Handle);
    Result := string(PChar(Result));
  end;
end;

// -----------------------------------------------------------------------------
// Load libvlc library into memory
// -----------------------------------------------------------------------------

function LoadVLCLibrary(APath: string): integer;
begin
  Result := LoadLibrary(PWideChar(APath + '\libvlccore.dll'));
  if Result <> 0 then
    Result := LoadLibrary(PWideChar(APath + '\libvlc.dll'));
end;

// -----------------------------------------------------------------------------

function GetAProcAddress(handle: integer; var addr: Pointer; procName: string; failedList: TStringList): integer;
begin
  addr := GetProcAddress(handle, PWideChar(procName));
  if Assigned(addr) then Result := 0
  else
  begin
    if Assigned(failedList) then failedList.Add(procName);
    Result := -1;
  end;
end;

// -----------------------------------------------------------------------------
// Get address of libvlc functions
// -----------------------------------------------------------------------------

function LoadVLCFunctions(vlcHandle: integer; failedList: TStringList): Boolean;
begin
  GetAProcAddress(vlcHandle, @libvlc_new, 'libvlc_new', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_new_location, 'libvlc_media_new_location', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_new_from_media, 'libvlc_media_player_new_from_media', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_new, 'libvlc_media_player_new', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_set_media, 'libvlc_media_player_set_media', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_media_release, 'libvlc_media_release', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_set_hwnd, 'libvlc_media_player_set_hwnd', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_play, 'libvlc_media_player_play', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_stop, 'libvlc_media_player_stop', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_release, 'libvlc_media_player_release', failedList);
  GetAProcAddress(vlcHandle, @libvlc_release, 'libvlc_release', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_is_playing, 'libvlc_media_player_is_playing', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_new_path, 'libvlc_media_new_path', failedList);

  // Added Kameleon
  GetAProcAddress(vlcHandle, @libvlc_media_player_get_time, 'libvlc_media_player_get_time', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_set_time, 'libvlc_media_player_set_time', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_get_length, 'libvlc_media_player_get_length', FailedList);

  GetAProcAddress(vlcHandle, @libvlc_media_player_set_pause, 'libvlc_media_player_set_pause', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_pause, 'libvlc_media_player_pause', FailedList);

  GetAProcAddress(vlcHandle, @libvlc_audio_get_volume, 'libvlc_audio_get_volume', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_audio_set_volume, 'libvlc_audio_set_volume', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_audio_set_mute, 'libvlc_audio_set_mute', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_audio_toggle_mute, 'libvlc_audio_toggle_mute', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_audio_get_track_count, 'libvlc_audio_get_track_count', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_audio_get_track, 'libvlc_audio_get_track', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_audio_set_track, 'libvlc_audio_set_track', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_audio_get_track_description, 'libvlc_audio_get_track_description', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_track_description_list_release, 'libvlc_track_description_list_release', FailedList);

  GetAProcAddress(vlcHandle, @libvlc_video_get_height, 'libvlc_video_get_height', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_video_get_width, 'libvlc_video_get_width', FailedList);

  GetAProcAddress(vlcHandle, @libvlc_media_player_get_fps, 'libvlc_media_player_get_fps', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_get_rate, 'libvlc_media_player_get_rate', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_set_rate, 'libvlc_media_player_set_rate', FailedList);

  GetAProcAddress(vlcHandle, @libvlc_video_get_spu_count, 'libvlc_video_get_spu_count', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_video_get_spu, 'libvlc_video_get_spu', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_video_set_spu, 'libvlc_video_set_spu', FailedList);

  GetAProcAddress(vlcHandle, @libvlc_video_set_mouse_input, 'libvlc_video_set_mouse_input', FailedList);
  GetAProcAddress(vlcHandle, @libvlc_video_set_key_input, 'libvlc_video_set_key_input', FailedList);

  // end Added Kameleon

  // if all functions loaded, result is an empty list, otherwise result is a list of functions failed
  Result := failedList.Count = 0;
end;

// ------------------------------------------------------------------------
//                          Public functions
// ------------------------------------------------------------------------

function VLC_Create: boolean;
var Sl: TStringList;
begin
  Result := false;
  Playing := false;

  // load vlc library
  vlclib := LoadVLCLibrary(GetVLCLibPath());
  if vlclib = 0 then
  begin
    //Showmessage('Load vlc library failed');
    Exit;
  end;

  // sL will contains list of functions fail to load
  Sl := TStringList.Create;
  if not LoadVLCFunctions(vlclib, Sl) then
  begin
    //Showmessage('Some VLC functions failed to load : ' + #13#10 + sL.Text);
    FreeLibrary(vlclib);
    Sl.Free;
    Exit;
  end;

  Sl.Free;

  // create new vlc instance
  vlcInstance := libvlc_new(0, nil);

  // create new vlc media player
  vlcMediaPlayer := libvlc_media_player_new(vlcInstance);

  libvlc_video_set_mouse_input(vlcMediaPlayer, 0); // both needed to make e.g. mouse events work..
  libvlc_video_set_key_input(vlcMediaPlayer, 0);   //

  vlcMediaPlayerInitialised := true;

  Result := true;
end;

procedure VLC_Destroy;
begin
  VLC_Stop;

  VlcMediaPlayerInitialised := false;

  // release vlc media player
  libvlc_media_player_release(vlcMediaPlayer);
  vlcMediaPlayer := nil;

  // release vlc instance
  libvlc_release(vlcInstance);

  // unload vlc library
  FreeLibrary(vlclib);

end;

function VLC_Play(File_: string; Panel_: TPanel): boolean;
var vlcMedia: plibvlc_media_t;
  spuCount, Res: integer;
begin
  Result := false;

  // create new vlc media from file
  vlcMedia := libvlc_media_new_path(vlcInstance, PAnsiChar(UTF8Encode(File_)) { PAnsiChar(File_)});

  // if you want to play from network, use libvlc_media_new_location instead
  // vlcMedia := libvlc_media_new_location(vlcInstance, 'udp://@225.2.1.27:5127');

  libvlc_media_player_set_media(vlcmediaplayer, vlcMedia);

  // now no need the vlc media, free it
  libvlc_media_release(vlcMedia);

  // play video in a TPanel, if not call this routine, vlc media will open a new window
  libvlc_media_player_set_hwnd(vlcMediaPlayer, Pointer(Panel_.Handle));

  // play media
  Res := libvlc_media_player_play(vlcMediaPlayer);

  if Res = 0 then // success
  begin

    repeat
      Sleep(100);
      Application.ProcessMessages;
    until libvlc_media_player_is_playing(vlcMediaPlayer) = 1;

    spuCount := libvlc_video_get_spu_Count(vlcMediaPlayer);
    if (SpuCount > 0) then libvlc_video_set_spu(vlcMediaPlayer, -1); // disable subtitleshowing

    libvlc_video_set_mouse_input(vlcMediaPlayer, 0); // does not work ???

    Playing := true;
    Result := Playing;
  end;
end;

procedure VLC_Stop;
begin
  if not Playing then exit;

  Playing := false;

  // stop vlc media player
  libvlc_media_player_stop(vlcMediaPlayer);

  // and wait until it completely stops
  while libvlc_media_player_is_playing(vlcMediaPlayer) = 1 do
  begin
    Sleep(100);
  end;
end;

function VLC_Playing: boolean;
begin
  Result := Playing;
end;

procedure VLC_Set_Volume(Volume_: integer);
begin
  libvlc_audio_set_volume(vlcMediaplayer, Volume_);
end;

function VLC_Get_Volume: integer;
begin
  Result := libvlc_audio_get_volume(vlcmediaplayer);
end;

procedure VLC_Set_Time(Time_: Int64);
begin
  libvlc_media_player_set_time(vlcMediaPlayer, Time_);
end;

function VLC_Get_Time: Int64;
begin
  Result := libvlc_media_player_get_time(VlcMediaPlayer);
end;

function VLC_Get_Length: Int64;
begin
  Result := libvlc_media_player_get_length(vlcMediaPlayer);
end;

procedure VLC_Set_Pause(on: boolean); // pause/resume
var Val: Integer;
begin
  if Playing then
  begin
    Val := 0;
    if on then Val := 1;
    libvlc_media_player_set_pause(VlcMediaPlayer, Val);
  end;
end;

procedure VLC_Toggle_Pause; // pause/resume
begin
  if Playing then
  begin
    libvlc_media_player_pause(VlcMediaPlayer);
  end;
end;

procedure VLC_Get_Audio_Track_Info(var List_: TAudioStreamList);
var libvlc_track_description: Plibvlc_track_description;
  P: Plibvlc_track_description;
  Name: string;
  Id: integer;
  N: integer;
begin
  setlength(List_, 0);

  if Playing then
  begin

    libvlc_track_description := libvlc_audio_get_track_description(vlcMediaPlayer);

    P := libvlc_track_description;
    while (P <> nil) do
    begin
      Id := P^.id; // get id
      Name := P^.name; // get name
      N := length(List_);
      SetLength(List_, N + 1);
      List_[N].Id := Id;
      List_[N].Name := Name;
      P := P^.next_; // next item in the list
    end;

    libvlc_track_description_list_release(libvlc_track_description); // release the pointer
  end;
end;

procedure VLC_Set_Audio_Track(Track_: integer);
var SL: TAudioStreamList;
  TrackId: integer;
begin
  if Playing then
  begin
    VLC_Get_Audio_Track_Info(SL);
    if Length(SL) > Track_ then
    begin
      TrackId := SL[Track_].Id;
      libvlc_audio_set_track(vlcMediaPlayer, TrackId);
    end;
    SetLength(SL, 0); // free sl
  end;
end;

procedure VLC_Set_Mute(on: boolean); // mute demute
var Val: integer;
begin
  if Playing then
  begin
    Val := 0;
    if on then Val := 1;
    libvlc_audio_set_mute(vlcMediaplayer, Val);
  end;
end;

procedure VLC_Toggle_Mute; // mute/demute
begin
  if Playing then
    libvlc_audio_toggle_mute(vlcMediaplayer);
end;

function VLC_Get_Width: integer;
begin
  Result := 0;
  if Playing then
    Result := libvlc_video_get_width(vlcmediaplayer);
end;

function VLC_Get_Height: integer;
begin
  Result := 0;
  if Playing then
    Result := libvlc_video_get_height(vlcmediaplayer);
end;

function VLC_Get_FPS: single;
begin
  Result := 0.0;
  if Playing then
    Result := libvlc_media_player_get_fps(vlcmediaplayer);
end;

procedure VLC_Set_Rate(Rate_: single);
begin
  if Playing then
    libvlc_media_player_set_rate(vlcmediaplayer, Rate_);
end;

function VLC_Get_Rate: single;
begin
  Result := 0.0;
  if Playing then
    Result := libvlc_media_player_get_rate(vlcmediaplayer);
end;

end.
