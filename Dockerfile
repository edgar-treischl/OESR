#FROM rocker/r-ver:4.4.1
FROM rocker/verse:4.4.1

# Set up the locale to avoid encoding issues in R, set to German locale
RUN echo "de_DE.UTF-8 UTF-8" > /etc/locale.gen && \
    locale-gen de_DE.UTF-8 && \
    update-locale LANG=de_DE.UTF-8
    


# Install R packages that are needed even before running renv::restore()
RUN  R -e "install.packages('renv', repos='https://cloud.r-project.org')"
   

# Install TinyTeX

# Install required LaTeX packages
RUN tlmgr option repository ctan
#RUN R -e "tinytex::tlmgr_install(c('pdflscape', 'environ', 'fp', 'pgf', 'tcolorbox', 'trimspaces'))"
RUN R -e "tinytex::tlmgr_update()"
RUN tlmgr option autobackup 0 

# Set the working directory
WORKDIR /app

# Copy project files into the container (includes renv.lock and renv directory)
COPY . .

# Run renv::restore() to restore R package dependencies
RUN R -e "renv::restore()"
RUN R -e "install.packages('svglite', repos='https://cloud.r-project.org')" 

# Set the environment variable for locale and TinyTeX path
ENV LANG=en_US.UTF-8
ENV PATH=/root/.TinyTeX/bin/x86_64-linux:$PATH



# Set the entry point to run your main R script
ENTRYPOINT ["Rscript", "prog/edgar.R"]
