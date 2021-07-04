extract-tracked
===============

This is a short utility I've written to offload some music from a
popular video service, retaining the “chapters” information to the
resulting Matroska container.  By parsing the video description with
regexes.

Usage
-----

    extract-tracked <video id>

Building
--------

This is slightly more complicated than it'd need, due to the `gogol`
suite of packages currently not building on stackage LTS at time of
writing.  So you're going to have to unpack `gogol-core-0.5.0` in the
directory, patch what's needed (I forgot :-( ).

Then:

    stack build
    
Running
-------

Runtime dependencies on:

* ffprobe (from ffmpeg)
* mkvmerge (from mkvtoolnix)
* youtube-dl

The trickiest part is obtaining credentials for the Google API.
Google documents it there:

https://cloud.google.com/iam/docs/creating-managing-service-account-keys

Put the resulting JSON in a file named `credentials.json` in the
working directory, or set `GOOGLE_APPLICATION_CREDENTIALS` to point to
it.  See the `gogol` documentation for more detail.

You may also configure the path to `youtube-dl` using environment
variable `YOUTUBEDL`.
