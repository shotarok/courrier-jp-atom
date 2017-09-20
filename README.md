# courrier-jp-atom

A [COURRiER JAPON](https://courrier.jp/) atom feed generator and uploader to a S3 bucket.

## Getting Started

Currently, a daily atom feed generated by this project is hosted here (https://courrier-japon.s3.amazonaws.com/atom.xml).

### Build

```
$ stack build
```

### Generate and Upload

```
$ env AWS_ACCESS_KEY_ID=YOUR_ACCESS_KEY \
      AWS_SECRET_ACCESS_KEY=YOUR_SECRET_KEY \
  stack exec courrier-jp-atom-exe
```

### Running tests

```
$ stack build --test
```

## Author

Shotaro Kohama - [shotarok](https://github.com/shotarok)

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details
