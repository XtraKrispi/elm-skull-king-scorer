var gulp    = require('gulp');
var elm     = require('gulp-elm');
var plumber = require('gulp-plumber');
var del     = require('del');
var sass    = require('gulp-sass');
var browserSync = require('browser-sync').create();
var jsonServer = require('gulp-json-srv').create({
    port: 8080
});

// builds elm files and static resources (i.e. html and css) from src to dist folder
var paths = {
    dest: 'dist',
    elmApp: 'src/App.elm',
    elm: 'src/**/*.elm',
    staticAssets: 'src/static/**/*.*',
    sass: 'src/sass/*.{sass,scss}'
};

gulp.task('clean', function(cb){
    del([paths.dest], cb);
});

gulp.task('elm-init', elm.init);

gulp.task('elm', ['elm-init'], function(){
    return gulp.src(paths.elmApp)
        .pipe(plumber())
        .pipe(elm())
        .pipe(gulp.dest(paths.dest))
        .pipe(browserSync.stream());
});

gulp.task('staticAssets', function(){
    return gulp.src(paths.staticAssets)
        .pipe(plumber())
        .pipe(gulp.dest(paths.dest));
});

gulp.task('sass', function(){
    return gulp.src(paths.sass)
        .pipe(sass().on('error', sass.logError))
        .pipe(gulp.dest(paths.dest + "/css"))
        .pipe(browserSync.stream());
});

gulp.task('browser-sync', function(){
    browserSync.init({
        server: {
            baseDir: "dist/"
        }
    });
});

gulp.task('elm-watch', ['elm'], browserSync.reload);

gulp.task('jsonServer', function(){
    return gulp.src('json-data.json')
        .pipe(jsonServer.pipe());
});

gulp.task('watch', ['browser-sync'], function(){
    gulp.watch(["json-data.json"], ["jsonServer"]).on('change', browserSync.reload);
    gulp.watch(paths.elm, ['elm-watch']);
    gulp.watch(paths.staticAssets, ['staticAssets']).on('change', browserSync.reload);
    gulp.watch(paths.sass, ['sass']);
});

gulp.task('build', ['elm', 'staticAssets', 'sass']);
gulp.task('dev', ['jsonServer', 'build', 'watch']);
gulp.task('default', ['build']);